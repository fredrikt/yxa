-module(eldap).
%%% --------------------------------------------------------------------
%%% Created:  12 Oct 2000 by Tobbe <tnt@home.se>
%%% Function: Erlang client LDAP implementation according RFC 2251.
%%%           The interface is based on RFC 1823, and
%%%           draft-ietf-asid-ldap-c-api-00.txt
%%%
%%% Copyright (C) 2000  Torbjörn Törnkvist, tnt@home.se
%%% 
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%%%
%%% --------------------------------------------------------------------
-vc('\$Id$ ').
-export([open/1,open/2,simple_bind/3,controlling_process/2,
	 baseObject/0,singleLevel/0,wholeSubtree/0,close/1,
	 equalityMatch/2,greaterOrEqual/2,lessOrEqual/2,
	 approxMatch/2,search/2,substrings/2,present/1,
	 'and'/1,'or'/1,'not'/1,modify/3, mod_add/2, mod_delete/2,
	 mod_replace/2, add/3, delete/2, modify_dn/5]).

-import(lists,[concat/1]).

-include("LDAPv3.hrl").
-include("eldap.hrl").

-define(LDAP_VERSION, 3).

-record(eldap, {version = ?LDAP_VERSION,
		host,         % Host running LDAP server
		port = 389,   % The LDAP server port
		fd,           % Socket filedescriptor.
		rootdn = "",  % Name of the entry to bind as
		passwd,       % Password for (above) entry
		id = 0,       % LDAP Request ID 
		log,          % User provided log function
		socketmodule = gen_tcp   % what Erlang module to use for network communication (gen_tcp or ssl)
	       }).

%%% For debug purposes
%%-define(PRINT(S, A), io:fwrite("~w(~w): " ++ S, [?MODULE,?LINE|A])).
-define(PRINT(S, A), true).

%%% ====================================================================
%%% Exported interface
%%% ====================================================================

%%% --------------------------------------------------------------------
%%% open(Hosts [,Opts] )
%%% --------------------
%%% Setup a connection to on of the Hosts in the argument
%%% list. Stop at the first successful connection attempt.
%%% Valid Opts are:      Where:
%%%
%%%    {port, Port}    - Port is the port number 
%%%    {log, F}        - F(LogLevel, FormatString, ListOfArgs)
%%%
%%% --------------------------------------------------------------------
open(Hosts) -> 
    open(Hosts, []).

open(Hosts, Opts) when list(Hosts), list(Opts) ->
    Self = self(),
    Pid = spawn_link(fun() -> init(Hosts, Opts, Self) end),
    recv(Pid).

%%% --------------------------------------------------------------------
%%% Shutdown connection (and process) asynchronous.
%%% --------------------------------------------------------------------

close(Handle) when pid(Handle) ->
    send(Handle, close).

%%% --------------------------------------------------------------------
%%% Set who we should link ourselves to
%%% --------------------------------------------------------------------

controlling_process(Handle, Pid) when pid(Handle),pid(Pid)  ->
    link(Pid),
    send(Handle, {cnt_proc, Pid}),
    recv(Handle).

%%% --------------------------------------------------------------------
%%% Authenticate ourselves to the Directory 
%%% using simple authentication.
%%%
%%%  Dn      -  The name of the entry to bind as
%%%  Passwd  -  The password to be used
%%%
%%%  Returns: ok | {error, Error}
%%% --------------------------------------------------------------------
simple_bind(Handle, Dn, Passwd) when pid(Handle)  ->
    send(Handle, {simple_bind, Dn, Passwd}),
    recv(Handle).

%%% --------------------------------------------------------------------
%%% Add an entry. The entry field MUST NOT exist for the AddRequest
%%% to succeed. The parent of the entry MUST exist.
%%% Example:
%%%
%%%  add(Handle, 
%%%         "cn=Bill Valentine, ou=people, o=Bluetail AB, dc=bluetail, dc=com",
%%%         [{"objectclass", ["person"]},
%%%          {"cn", ["Bill Valentine"]},
%%%          {"sn", ["Valentine"]},
%%%          {"telephoneNumber", ["545 555 00"]}]
%%%     )
%%% --------------------------------------------------------------------
add(Handle, Entry, Attributes) when pid(Handle),list(Entry),list(Attributes) ->
    send(Handle, {add, Entry, add_attrs(Attributes)}),
    recv(Handle).

%%% Do sanity check !
add_attrs(Attrs) ->
    F = fun({Type,Vals}) when list(Type),list(Vals) -> 
		%% Confused ? Me too... :-/
		{'AddRequest_attributes',Type, Vals} 
	end,
    case catch lists:map(F, Attrs) of
	{'EXIT', _} -> throw({error, attribute_values});
	Else        -> Else
    end.

%%% --------------------------------------------------------------------
%%% Delete an entry. The entry consists of the DN of 
%%% the entry to be deleted.
%%% Example:
%%%
%%%  delete(Handle, 
%%%         "cn=Bill Valentine, ou=people, o=Bluetail AB, dc=bluetail, dc=com"
%%%        )
%%% --------------------------------------------------------------------
delete(Handle, Entry) when pid(Handle), list(Entry) ->
    send(Handle, {delete, Entry}),
    recv(Handle).

%%% --------------------------------------------------------------------
%%% Modify an entry. Given an entry a number of modification
%%% operations can be performed as one atomic operation.
%%% Example:
%%%
%%%  modify(Handle, 
%%%         "cn=Torbjorn Tornkvist, ou=people, o=Bluetail AB, dc=bluetail, dc=com",
%%%         [replace("telephoneNumber", ["555 555 00"]),
%%%          add("description", ["LDAP hacker"])] 
%%%        )
%%% --------------------------------------------------------------------
modify(Handle, Object, Mods) when pid(Handle), list(Object), list(Mods) ->
    send(Handle, {modify, Object, Mods}),
    recv(Handle).

%%%
%%% Modification operations. 
%%% Example:
%%%            replace("telephoneNumber", ["555 555 00"])
%%%
mod_add(Type, Values) when list(Type), list(Values)     -> m(add, Type, Values).
mod_delete(Type, Values) when list(Type), list(Values)  -> m(delete, Type, Values).
mod_replace(Type, Values) when list(Type), list(Values) -> m(replace, Type, Values).

m(Operation, Type, Values) ->
    #'ModifyRequest_modification_SEQOF'{
       operation = Operation,
       modification = #'AttributeTypeAndValues'{
	 type = Type,
	 vals = Values}}.

%%% --------------------------------------------------------------------
%%% Modify an entry. Given an entry a number of modification
%%% operations can be performed as one atomic operation.
%%% Example:
%%%
%%%  modify_dn(Handle, 
%%%    "cn=Bill Valentine, ou=people, o=Bluetail AB, dc=bluetail, dc=com",
%%%    "cn=Ben Emerson",
%%%    true,
%%%    ""
%%%        )
%%% --------------------------------------------------------------------
modify_dn(Handle, Entry, NewRDN, DelOldRDN, NewSup) 
  when pid(Handle),list(Entry),list(NewRDN),atom(DelOldRDN),list(NewSup) ->
    send(Handle, {modify_dn, Entry, NewRDN, 
		  bool_p(DelOldRDN), optional(NewSup)}),
    recv(Handle).

%%% Sanity checks !

bool_p(Bool) when Bool==true;Bool==false -> Bool.

optional([])    -> asn1_NOVALUE;
optional(Value) -> Value.

%%% --------------------------------------------------------------------
%%% Synchronous search of the Directory returning a 
%%% requested set of attributes.
%%%
%%%  Example:
%%%
%%%	Filter = eldap:substrings("sn", [{any,"o"}]),
%%%	eldap:search(S, [{base, "dc=bluetail, dc=com"},
%%%	                 {filter, Filter},
%%%			 {attributes,["cn"]}])),
%%%
%%% Returned result:  {ok, #eldap_search_result{}}
%%%
%%% Example:
%%%
%%%  {ok,{eldap_search_result,
%%%        [{eldap_entry,
%%%           "cn=Magnus Froberg, dc=bluetail, dc=com",
%%%           [{"cn",["Magnus Froberg"]}]},
%%%         {eldap_entry,
%%%           "cn=Torbjorn Tornkvist, dc=bluetail, dc=com",
%%%           [{"cn",["Torbjorn Tornkvist"]}]}],
%%%        []}}
%%%
%%% --------------------------------------------------------------------
search(Handle, A) when pid(Handle), record(A, eldap_search) ->
    call_search(Handle, A);
search(Handle, L) when pid(Handle), list(L) ->
    case catch parse_search_args(L) of
	{error, Emsg}                  -> {error, Emsg};
	A when record(A, eldap_search) -> call_search(Handle, A)
    end.

call_search(Handle, A) ->
    send(Handle, {search, A}),
    recv(Handle).

parse_search_args(Args) ->
    parse_search_args(Args, #eldap_search{scope = wholeSubtree}).
    
parse_search_args([{base, Base}|T],A) ->
    parse_search_args(T,A#eldap_search{base = Base});
parse_search_args([{filter, Filter}|T],A) ->
    parse_search_args(T,A#eldap_search{filter = Filter});
parse_search_args([{scope, Scope}|T],A) ->
    parse_search_args(T,A#eldap_search{scope = Scope});
parse_search_args([{attributes, Attrs}|T],A) ->
    parse_search_args(T,A#eldap_search{attributes = Attrs});
parse_search_args([{types_only, TypesOnly}|T],A) ->
    parse_search_args(T,A#eldap_search{types_only = TypesOnly});
parse_search_args([{timeout, Timeout}|T],A) when integer(Timeout) ->
    parse_search_args(T,A#eldap_search{timeout = Timeout});
parse_search_args([H|T],A) ->
    throw({error,{unknown_arg, H}});
parse_search_args([],A) ->
    A.

%%%
%%% The Scope parameter
%%%
baseObject()   -> baseObject.
singleLevel()  -> singleLevel.
wholeSubtree() -> wholeSubtree.

%%%
%%% Boolean filter operations
%%%
'and'(ListOfFilters) when list(ListOfFilters) -> {'and',ListOfFilters}.
'or'(ListOfFilters)  when list(ListOfFilters) -> {'or', ListOfFilters}.
'not'(Filter)        when tuple(Filter)       -> {'not',Filter}.

%%%
%%% The following Filter parameters consist of an attribute
%%% and an attribute value. Example: F("uid","tobbe")
%%%
equalityMatch(Desc, Value)   -> {equalityMatch, av_assert(Desc, Value)}.
greaterOrEqual(Desc, Value)  -> {greaterOrEqual, av_assert(Desc, Value)}.
lessOrEqual(Desc, Value)     -> {lessOrEqual, av_assert(Desc, Value)}.
approxMatch(Desc, Value)     -> {approxMatch, av_assert(Desc, Value)}.

av_assert(Desc, Value) ->
    #'AttributeValueAssertion'{attributeDesc  = Desc,
			       assertionValue = Value}.

%%%
%%% Filter to check for the presence of an attribute
%%%
present(Attribute) when list(Attribute) -> 
    {present, Attribute}.


%%%
%%% A substring filter seem to be based on a pattern:
%%%
%%%   InitValue*AnyValue*FinalValue
%%%
%%% where all three parts seem to be optional (at least when
%%% talking with an OpenLDAP server). Thus, the arguments
%%% to substrings/2 looks like this:
%%%
%%% Type   ::= string( <attribute> )
%%% SubStr ::= listof( {initial,Value} | {any,Value}, {final,Value})
%%%
%%% Example: substrings("sn",[{initial,"To"},{any,"kv"},{final,"st"}])
%%% will match entries containing:  'sn: Tornkvist'
%%%
substrings(Type, SubStr) when list(Type), list(SubStr) -> 
    Ss = {'SubstringFilter_substrings',v_substr(SubStr)},
    {substrings,#'SubstringFilter'{type = Type,
				   substrings = Ss}}.
    
%%% --------------------------------------------------------------------
%%% Worker process. We keep track of a controlling process to
%%% be able to terminate together with it.
%%% --------------------------------------------------------------------

init(Hosts, Opts, Cpid) ->
    Data = parse_args(Opts, Cpid, #eldap{}),
    case try_connect(Hosts, Data) of
	{ok,Data2} ->
	    send(Cpid, {ok,self()}),
	    loop(Cpid, Data2);
	Else ->
 	    send(Cpid, Else),
	    unlink(Cpid),
	    exit(Else)
    end.

parse_args([{port, Port}|T], Cpid, Data) when integer(Port) ->
    parse_args(T, Cpid, Data#eldap{port = Port});
parse_args([{use_ssl, true}|T], Cpid, Data) ->
    parse_args(T, Cpid, Data#eldap{port = 636, socketmodule = ssl});
parse_args([{use_ssl, false}|T], Cpid, Data) ->
    parse_args(T, Cpid, Data);
parse_args([{log, F}|T], Cpid, Data) when function(F) ->
    parse_args(T, Cpid, Data#eldap{log = F});
parse_args([{log, _}|T], Cpid, Data) ->
    parse_args(T, Cpid, Data);
parse_args([H|T], Cpid, _) ->
    send(Cpid, {error,{wrong_option,H}}),
    exit(wrong_option);
parse_args([], _, Data) ->
    Data.
		  
%%% Try to connect to the hosts in the listed order,
%%% and stop with the first one to which a successful
%%% connection is made.

try_connect([Host|Hosts], Data) ->
    TcpOpts = [{packet, asn1}, {active,false}, {nodelay, true}],
    SocketModule = Data#eldap.socketmodule,
    case SocketModule:connect(Host, Data#eldap.port, TcpOpts) of
	{ok,Fd} -> {ok,Data#eldap{host = Host, fd   = Fd}};
	_       -> try_connect(Hosts, Data)
    end;
try_connect([],_) ->
    {error,"connect failed"}.

    
loop(Cpid, Data) ->
    receive

	{From, {search, A}} ->
	    {Res,NewData} = do_search(Data, A),
	    send(From,Res),
	    loop(Cpid, NewData);

	{From, {modify, Obj, Mod}} ->
	    {Res,NewData} = do_modify(Data, Obj, Mod),
	    send(From,Res),
	    loop(Cpid, NewData);

	{From, {modify_dn, Obj, NewRDN, DelOldRDN, NewSup}} ->
	    {Res,NewData} = do_modify_dn(Data, Obj, NewRDN, DelOldRDN, NewSup),
	    send(From,Res),
	    loop(Cpid, NewData);

	{From, {add, Entry, Attrs}} ->
	    {Res,NewData} = do_add(Data, Entry, Attrs),
	    send(From,Res),
	    loop(Cpid, NewData);

	{From, {delete, Entry}} ->
	    {Res,NewData} = do_delete(Data, Entry),
	    send(From,Res),
	    loop(Cpid, NewData);

	{From, {simple_bind, Dn, Passwd}} ->
	    {Res,NewData} = do_simple_bind(Data, Dn, Passwd),
	    send(From,Res),
	    loop(Cpid, NewData);

	{From, {cnt_proc, NewCpid}} ->
	    unlink(Cpid),
	    send(From,ok),
	    ?PRINT("New Cpid is: ~p~n",[NewCpid]),
	    loop(NewCpid, Data);

	{From, close} ->
	    unlink(Cpid),
	    exit(closed);

	{Cpid, 'EXIT', Reason} ->
	    ?PRINT("Got EXIT from Cpid, reason=~p~n",[Reason]),
	    exit(Reason);

	_XX ->
	    ?PRINT("loop got: ~p~n",[_XX]),
	    loop(Cpid, Data)

    end.

%%% --------------------------------------------------------------------
%%% bindRequest
%%% --------------------------------------------------------------------

%%% Authenticate ourselves to the directory using
%%% simple authentication.

do_simple_bind(Data, Dn, Passwd) ->
    case catch exec_simple_bind(Data#eldap{rootdn = Dn, 
					   passwd = Passwd,
					   id     = bump_id(Data)}) of
	{ok,NewData} -> {ok,NewData};
	{error,Emsg} -> {{error,Emsg},Data};
	Else         -> {{error,Else},Data}
    end.

exec_simple_bind(Data) ->
    Req = #'BindRequest'{version        = Data#eldap.version,
			 name           = Data#eldap.rootdn,  
			 authentication = {simple, Data#eldap.passwd}},
    log2(Data, "bind request = ~p~n", [Req]),
    Reply = request(Data#eldap.socketmodule, Data#eldap.fd, Data#eldap.id, {bindRequest, Req}),
    log2(Data, "bind reply = ~p~n", [Reply]),    
    exec_simple_bind_reply(Data, Reply).

exec_simple_bind_reply(Data, {ok,Msg}) when 
  Msg#'LDAPMessage'.messageID == Data#eldap.id ->
    case Msg#'LDAPMessage'.protocolOp of
	{bindResponse, Result} ->
	    case Result#'LDAPResult'.resultCode of
		success -> {ok,Data};
		Error   -> {error, Error}
	    end;
	Other -> {error, Other}
    end;
exec_simple_bind_reply(Data, Error) ->
    {error, Error}.


%%% --------------------------------------------------------------------
%%% searchRequest
%%% --------------------------------------------------------------------

do_search(Data, A) ->
    case catch do_search_0(Data, A) of
	{error,Emsg}         -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error}       -> {ldap_closed_p(Data, Error),Data};
	{ok,Res,Ref,NewData} -> {{ok,polish(Res, Ref)},NewData};
	Else                 -> {ldap_closed_p(Data, Else),Data}
    end.

%%%
%%% Polish the returned search result
%%%

polish(Res, Ref) ->
    R = polish_result(Res),
    %%% No special treatment of referrals at the moment.
    #eldap_search_result{entries = R,
			 referrals = Ref}.

polish_result([H|T]) when record(H, 'SearchResultEntry') ->
    ObjectName = H#'SearchResultEntry'.objectName,
    F = fun({_,A,V}) -> {A,V} end,
    Attrs = lists:map(F, H#'SearchResultEntry'.attributes),
    [#eldap_entry{object_name = ObjectName,
		  attributes  = Attrs}|
     polish_result(T)];
polish_result([]) ->
    [].

do_search_0(Data, A) ->
    Req = #'SearchRequest'{baseObject = A#eldap_search.base,
			   scope = v_scope(A#eldap_search.scope),
			   derefAliases = neverDerefAliases,
			   sizeLimit = 0, % no size limit
			   timeLimit = v_timeout(A#eldap_search.timeout),
			   typesOnly = v_bool(A#eldap_search.types_only),
			   filter = v_filter(A#eldap_search.filter),
			   attributes = v_attributes(A#eldap_search.attributes)
			  },
    Id = bump_id(Data),
    collect_search_responses(Data#eldap{id=Id}, Req, Id).
    
%%% The returned answers cames in one packet per entry
%%% mixed with possible referals

collect_search_responses(Data, Req, ID) ->
    S = Data#eldap.fd,
    log2(Data, "search request = ~p~n", [Req]),
    send_request(Data#eldap.socketmodule, S, ID, {searchRequest, Req}),
    Resp = recv_response(Data#eldap.socketmodule, S),
    log2(Data, "search reply = ~p~n", [Resp]),    
    collect_search_responses(Data, S, ID, Resp, [], []).

collect_search_responses(Data, S, ID, {ok,Msg}, Acc, Ref) 
  when record(Msg,'LDAPMessage') ->
    case Msg#'LDAPMessage'.protocolOp of
	{'searchResDone',R} when R#'LDAPResult'.resultCode == success ->
	    log2(Data, "search reply = searchResDone ~n", []),    
	    {ok,Acc,Ref,Data};
	{'searchResEntry',R} when record(R,'SearchResultEntry') ->
	    Resp = recv_response(Data#eldap.socketmodule, S),
	    log2(Data, "search reply = ~p~n", [Resp]),    
	    collect_search_responses(Data, S, ID, Resp, [R|Acc], Ref);
	{'searchResRef',R} ->
	    %% At the moment we don't do anyting sensible here since
	    %% I haven't been able to trigger the server to generate
	    %% a response like this.
	    Resp = recv_response(Data#eldap.socketmodule, S),
	    log2(Data, "search reply = ~p~n", [Resp]),    
	    collect_search_responses(Data, S, ID, Resp, Acc, [R|Ref]);
	Else ->
	    throw({error,Else})
    end;
collect_search_responses(Data, S, ID, Else, Acc, Ref) ->
    throw({error,Else}).

%%% --------------------------------------------------------------------
%%% addRequest
%%% --------------------------------------------------------------------

do_add(Data, Entry, Attrs) ->
    case catch do_add_0(Data, Entry, Attrs) of
	{error,Emsg}   -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error} -> {ldap_closed_p(Data, Error),Data};
	{ok,NewData}   -> {ok,NewData};
	Else           -> {ldap_closed_p(Data, Else),Data}
    end.

do_add_0(Data, Entry, Attrs) ->
    Req = #'AddRequest'{entry = Entry,
			attributes = Attrs},
    S = Data#eldap.fd,
    Id = bump_id(Data),
    log2(Data, "add request = ~p~n", [Req]),
    Resp = request(Data#eldap.socketmodule, S, Id, {addRequest, Req}),
    log2(Data, "add reply = ~p~n", [Resp]),    
    check_reply(Data#eldap{id = Id}, Resp, addResponse).


%%% --------------------------------------------------------------------
%%% deleteRequest
%%% --------------------------------------------------------------------

do_delete(Data, Entry) ->
    case catch do_delete_0(Data, Entry) of
	{error,Emsg}   -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error} -> {ldap_closed_p(Data, Error),Data};
	{ok,NewData}   -> {ok,NewData};
	Else           -> {ldap_closed_p(Data, Else),Data}
    end.

do_delete_0(Data, Entry) ->
    S = Data#eldap.fd,
    Id = bump_id(Data),
    log2(Data, "del request = ~p~n", [Entry]),
    Resp = request(Data#eldap.socketmodule, S, Id, {delRequest, Entry}),
    log2(Data, "del reply = ~p~n", [Resp]),    
    check_reply(Data#eldap{id = Id}, Resp, delResponse).


%%% --------------------------------------------------------------------
%%% modifyRequest
%%% --------------------------------------------------------------------

do_modify(Data, Obj, Mod) ->
    case catch do_modify_0(Data, Obj, Mod) of
	{error,Emsg}   -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error} -> {ldap_closed_p(Data, Error),Data};
	{ok,NewData}   -> {ok,NewData};
	Else           -> {ldap_closed_p(Data, Else),Data}
    end.

do_modify_0(Data, Obj, Mod) ->
    v_modifications(Mod),
    Req = #'ModifyRequest'{object = Obj,
			   modification = Mod},
    S = Data#eldap.fd,
    Id = bump_id(Data),
    log2(Data, "modify request = ~p~n", [Req]),
    Resp = request(Data#eldap.socketmodule, S, Id, {modifyRequest, Req}),
    log2(Data, "modify reply = ~p~n", [Resp]),    
    check_reply(Data#eldap{id = Id}, Resp, modifyResponse).

%%% --------------------------------------------------------------------
%%% modifyDNRequest
%%% --------------------------------------------------------------------

do_modify_dn(Data, Entry, NewRDN, DelOldRDN, NewSup) ->
    case catch do_modify_dn_0(Data, Entry, NewRDN, DelOldRDN, NewSup) of
	{error,Emsg}   -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error} -> {ldap_closed_p(Data, Error),Data};
	{ok,NewData}   -> {ok,NewData};
	Else           -> {ldap_closed_p(Data, Else),Data}
    end.

do_modify_dn_0(Data, Entry, NewRDN, DelOldRDN, NewSup) ->
    Req = #'ModifyDNRequest'{entry = Entry,
			     newrdn = NewRDN,
			     deleteoldrdn = DelOldRDN,
			     newSuperior = NewSup},
    S = Data#eldap.fd,
    Id = bump_id(Data),
    log2(Data, "modify DN request = ~p~n", [Req]),
    Resp = request(Data#eldap.socketmodule, S, Id, {modDNRequest, Req}),
    log2(Data, "modify DN reply = ~p~n", [Resp]),    
    check_reply(Data#eldap{id = Id}, Resp, modDNResponse).

%%% --------------------------------------------------------------------
%%% Send an LDAP request and receive the answer
%%% --------------------------------------------------------------------

request(SocketModule, S, ID, Request) ->
    send_request(SocketModule, S, ID, Request),
    recv_response(SocketModule, S).

send_request(SocketModule, S, ID, Request) ->
    Message = #'LDAPMessage'{messageID  = ID,
			     protocolOp = Request},
    {ok,Bytes} = asn1rt:encode('LDAPv3', 'LDAPMessage', Message),
    SocketModule:send(S, Bytes).

recv_response(SocketModule, S) ->
    case SocketModule:recv(S, 0) of
	{ok, Data} ->
	    check_tag(Data),
	    case asn1rt:decode('LDAPv3', 'LDAPMessage', Data) of
		{ok,Resp} -> {ok,Resp};
		Error     -> throw(Error)
	    end;
	Error ->
	    throw(Error)
    end.

%%% Sanity check of received packet
check_tag(Data) ->
    ok.
%    case asn1rt_ber:decode_tag(Data) of
%	{Tag, Data1, Rb} ->
%	    case asn1rt_ber:decode_length(Data1) of
%		{{Len,Data2}, Rb2} -> ok;
%		_ -> throw({error,decoded_tag_length})
%	    end;
%	_ -> throw({error,decoded_tag})
%    end.

%%% Check for expected kind of reply
check_reply(Data, {ok,Msg}, Op) when 
  Msg#'LDAPMessage'.messageID == Data#eldap.id ->
    case Msg#'LDAPMessage'.protocolOp of
	{Op, Result} ->
	    case Result#'LDAPResult'.resultCode of
		success -> {ok,Data};
		Error   -> {error, Error}
	    end;
	Other -> {error, Other}
    end;
check_reply(Data, Error, _) ->
    {error, Error}.


%%% --------------------------------------------------------------------
%%% Verify the input data
%%% --------------------------------------------------------------------

v_filter({'and',L})           -> {'and',L};
v_filter({'or', L})           -> {'or',L};
v_filter({'not',L})           -> {'not',L};
v_filter({equalityMatch,AV})  -> {equalityMatch,AV};
v_filter({greaterOrEqual,AV}) -> {greaterOrEqual,AV};
v_filter({lessOrEqual,AV})    -> {lessOrEqual,AV};
v_filter({approxMatch,AV})    -> {approxMatch,AV};
v_filter({present,A})         -> {present,A};
v_filter({substrings,S}) when record(S,'SubstringFilter') -> {substrings,S};
v_filter(_Filter) -> throw({error,concat(["unknown filter: ",_Filter])}).

v_modifications(Mods) ->
    F = fun({_,Op,_}) ->
		case lists:member(Op,[add,delete,replace]) of
		    true -> true;
		    _    -> throw({error,{mod_operation,Op}})
		end
	end,
    lists:foreach(F, Mods).

v_substr([{Key,Str}|T]) when list(Str),Key==initial;Key==any;Key==final ->
    [{Key,Str}|v_substr(T)];
v_substr([H|T]) ->
    throw({error,{substring_arg,H}});
v_substr([]) -> 
    [].
v_scope(baseObject)   -> baseObject;
v_scope(singleLevel)  -> singleLevel;
v_scope(wholeSubtree) -> wholeSubtree;
v_scope(_Scope)       -> throw({error,concat(["unknown scope: ",_Scope])}).

v_bool(true)  -> true;
v_bool(false) -> false;
v_bool(_Bool) -> throw({error,concat(["not Boolean: ",_Bool])}).

v_timeout(I) when integer(I), I>=0 -> I;
v_timeout(_I) -> throw({error,concat(["timeout not positive integer: ",_I])}).

v_attributes(Attrs) ->
    F = fun(A) when list(A) -> A;
	   (A) -> throw({error,concat(["attribute not String: ",A])})
	end,
    lists:map(F,Attrs).


%%% --------------------------------------------------------------------
%%% Log routines. Call a user provided log routine F.
%%% --------------------------------------------------------------------

log1(Data, Str, Args) -> log(Data, Str, Args, 1).
log2(Data, Str, Args) -> log(Data, Str, Args, 2).

log(Data, Str, Args, Level) when function(Data#eldap.log) ->
    catch (Data#eldap.log)(Level, Str, Args);
log(_, _, _, _) -> 
    ok.


%%% --------------------------------------------------------------------
%%% Misc. routines
%%% --------------------------------------------------------------------

send(To,Msg) -> To ! {self(),Msg}.
recv(From)   -> receive {From,Msg} -> Msg end.

ldap_closed_p(#eldap{socketmodule=ssl}=Data, Emsg) ->
    %% Check if the SSL socket seems to be alive or not
    case catch ssl:sockname(Data#eldap.fd) of
	{error, _} ->
	    ssl:close(Data#eldap.fd),
	    {error, ldap_closed};
	{ok, _} ->
	    {error, Emsg};
	_ ->
	    %% sockname crashes if the socket pid is not alive
	    {error, ldap_closed}
    end;

ldap_closed_p(Data, Emsg) ->
    %% non-SSL socket
    case inet:port(Data#eldap.fd) of
	{error,_} -> {error, ldap_closed};
	_         -> {error,Emsg}
    end.
    
bump_id(Data) -> Data#eldap.id + 1.

    

