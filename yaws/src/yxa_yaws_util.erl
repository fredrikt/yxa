%%%-------------------------------------------------------------------
%%% File    : yxa_yaws.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Yxa Yaws utility functions.
%%%
%%% Created : 11 Jun 2005 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(yxa_yaws_util).

%% General functions
-export([
	 script_output/1,
	 get_var/2,
	 get_var_int/2,
	 error/1,
	 hidden_inputs/1,
	 fmt_expires/1
	]).

%% Checkbox functions
-export([
	 form_checkboxes/3,
	 get_checkbox_input/2
	]).

%% Radio buttons
-export([
	 make_radio_input/4
	]).

%% userdb related functions
-export([
	 user_exists/2,
	 get_user_addresses/2
	]).

%% Ops functions
-export([
	 get_yxa_application_node/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("phone.hrl").
-include_lib("kernel/include/inet.hrl").


%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: script_output(In)
%% Descrip.:
%% Returns : EHTML = term(), Yaws ehtml data
%%--------------------------------------------------------------------
script_output({redirect, Link}) when is_list(Link) ->
    yaws_api:redirect(Link);
script_output(In) when is_list(In); is_tuple(In) ->
    DocRoot =
	case get({yxa_yaws_util, docroot}) of
	    undefined ->
		{ehtml, [{h1, [], "You forgot to put docroot in the process dictionary"}]};
	    DocRoot1 when is_list(DocRoot1) ->
		DocRoot1
	end,

    {html, Head1} = yaws_api:ssi(DocRoot, ["/HEAD"]),
    Head = {html, [], Head1},
    EHTML = [Head,
	     {h1, [], "Yxa administrative web interface"},
	     In],

    {ehtml, EHTML}.


%%--------------------------------------------------------------------
%% Function: form_checkboxes(A, In, List)
%%           A    = term(), Yaws request data
%%           In   = list() of atom(), current information
%%           List = {VarName, Fields}
%%             VarName = string(), the base of the HTTP variables
%%             Fields  = list() of atom(), the fields
%% Descrip.: Create checkboxes for every entry in Fields. Example :
%%             In = [foo]
%%             List = {"base", [foo, bar]}
%%           Outputs :
%%             checkbox "base.foo", checked
%%             checkbox "base.bar", not checked
%% Returns : HTML = term(), Yaws html data
%%--------------------------------------------------------------------
form_checkboxes(A, In, List) ->
    form_checkboxes(A, In, List, []).

form_checkboxes(A, In, {VarName, [H | T]}, Res) ->
    Key = lists:concat([VarName, ".", H]),
    Checked = case get_var(A, Key) of
		  {ok, "true"} -> [{checked, true}];
		  {ok, "false"} ->
		      [];
		  undefined ->
		      %% not enabled in form, check if it was enabled in Mnesia
		      case lists:member(H, In) of
			  true ->
			      [{checked, true}];
			  false ->
			      []
		      end
	      end,
    This =
	{br, [],
	 {input, [{type, checkbox},
		  {name, Key},
		  {value, "true"}
		 ] ++ Checked,
	  [atom_to_list(H)]}
	},
    form_checkboxes(A, In, {VarName, T}, [This | Res]);
form_checkboxes(_A, _In, {_VarName, []}, Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% Function: get_checkbox_input(A, In)
%%           A    = term(), Yaws request data
%%           In   = {VarName, Fields}
%%             VarName = string(), the base of the HTTP variables
%%             Fields  = list() of atom(), the fields
%% Descrip.: Check which of the HTML variables 'listed' in In that are
%%           set in A, and return a list of the atoms in Fields that
%%           are set. Example :
%%             VarName = "base"
%%              Fields = [foo, bar]
%%           returns [foo] if "base.foo" is set to "true", but
%%           "base.bar" is not.
%% Returns : list() of atom()
%%--------------------------------------------------------------------
get_checkbox_input(A, In) ->
    get_checkbox_input(A, In, []).

get_checkbox_input(A, {VarName, [H | T]}, Res) ->
    Key = lists:concat([VarName, ".", H]),
    case get_var(A, Key) of
	{ok, "true"} ->
	    get_checkbox_input(A, {VarName, T}, [H | Res]);
	undefined ->
	    get_checkbox_input(A, {VarName, T}, Res)
    end;
get_checkbox_input(_A, {_VarName, []}, Res) ->
    lists:reverse(Res).



%%--------------------------------------------------------------------
%% Function: make_radio_input(A, Name, Default, In)
%%           A       = term(), Yaws request data
%%           Name    = string(), variable name
%%           Default = string(), the radio button that should be
%%                     checked
%%           In      = list() of Value
%%             Value = string()
%% Descrip.: Return a Yaws HTML construct of a radio button input.
%% Returns : {ok, HTML}
%%--------------------------------------------------------------------
make_radio_input(A, Name, Default, In) ->
    make_radio_input2(A, Name, Default, In, false, []).

make_radio_input2(A, Name, Default, [Default | T], DefaultSeen, Res) ->
    %% The default one
    case DefaultSeen of
	true ->
	    erlang:error("invalid input", [Name, Default, [Default | T], DefaultSeen, Res]);
	false ->
	    ok
    end,
    This = {br, [], [{input, [{name, Name},
			       {type, "radio"},
			       {value, Default},
			       {checked, "true"}]
		       }, Default]
	   },
    make_radio_input2(A, Name, Default, T, true, [This | Res]);
make_radio_input2(A, Name, Default, [H | T], DefaultSeen, Res) ->
    %% Not the default one
    This = {br, [], [{input, [{name, Name},
			       {type, "radio"},
			       {value, H}]
		       }, H]
	   },
    make_radio_input2(A, Name, Default, T, DefaultSeen, [This | Res]);
make_radio_input2(_A, _Name, _Default, [], _DefaultSeen, Res) ->
    {ok, lists:reverse(Res)}.



%%--------------------------------------------------------------------
%% Function: hidden_inputs(In)
%%           In = list() of {Name, Value}
%%             Name  = string() | atom()
%%             Value = string() | atom()
%% Descrip.: Produce a list of input fields of type 'hidden'.
%% Returns : Out = list() of tuple()
%%--------------------------------------------------------------------
hidden_inputs(In) when is_list(In) ->
    hidden_inputs2(In, []).

hidden_inputs2([{Name, Value} | T], Res) ->
    This = {input, [{type, hidden},
		    {name, Name},
		    {value, Value}]},
    hidden_inputs2(T, [This | Res]);
hidden_inputs2([], Res) ->
    lists:reverse(Res).


%%--------------------------------------------------------------------
%% Function: get_var(A, Name)
%%           A    = term(), Yaws request data
%%           Name = string(), URL or POST data variable name
%% Descrip.: Get variable Name content. First look for a URL variable,
%%           and if that is not found and this is a POST, look for a
%%           posted variable.
%% Returns : {ok, Value} | undefined
%%--------------------------------------------------------------------
get_var(A, Name) when is_list(Name) ->
    case yaws_api:queryvar(A, Name) of
	{ok, QValue} when is_list(QValue) ->
	    {ok, QValue};
	_ ->
	    Method = case get({yxa_yaws_util, method}) of
			 undefined ->
			     throw({error, "yxa_yaws_util:get_var/2 could not get method"});
			 Method1 when is_atom(Method1) ->
			     Method1
		     end,

	    case Method of
		'POST' ->
		    case yaws_api:postvar(A, Name) of
			{ok, PValue} when is_list(PValue) ->
			    {ok, PValue};
			_ ->
			    undefined
		    end;
		_Method ->
		    undefined
	    end
    end.

%%--------------------------------------------------------------------
%% Function: get_var_int(A, Name)
%%           A    = term(), Yaws request data
%%           Name = string(), URL or POST data variable name
%% Descrip.: Like get_var/2 but converts the result to an integer.
%% Returns : {ok, Int} |
%%           throw({error, Reason})
%%           Int    = integer()
%%           Reason = string()
%%--------------------------------------------------------------------
get_var_int(A, Name) ->
    case get_var(A, Name) of
	{ok, Value} when is_list(Value) ->
	    %% test that it is something we can turn into an integer
	    try list_to_integer(Value) of
		Int when is_integer(Int) ->
		    {ok, Int}
	    catch
		_: _ ->
		    Msg = io_lib:format("Variable ~p invalid (~p is not an integer)",
					[Name, Value]),
		    throw({error, Msg})
	    end;
	_ ->
	    throw({error, "Invalid '" ++ Name ++ "' value"})
    end.


%%--------------------------------------------------------------------
%% Function: error(Msg)
%%           Msg = string()
%% Descrip.: Create a red error message of Msg.
%% Returns : HTML = term(), Yaws html data
%%--------------------------------------------------------------------
error(Msg) when is_list(Msg) ->
    {font,
     [{size, 4}, {color, red}],
     [{strong, [], ["ERROR: "]}, Msg]
    }.


%%--------------------------------------------------------------------
%% Function: user_exists(User, Node)
%%           User    = string(), username
%%           Node    = atom(), node we are to talk with
%% Descrip.: Check if user User exists in the Mnesia userdb.
%% Returns : true | false |
%%           throw({error, Reason})
%%           Reason = string()
%%--------------------------------------------------------------------
user_exists(User, Node) when is_list(User), is_atom(Node) ->
    case rpc:call(Node, phone, get_user, [User]) of
	{atomic, [{_Password, _Flags, _Classes}]} ->
	    true;
	{atomic, []} ->
	    false;
	E ->
	    Msg = io_lib:format("Failed checking if user ~p exists (node ~p) : ~p",
				[User, Node, E]),
	    throw({error, Msg})
    end.

%%--------------------------------------------------------------------
%% Function: get_user_addresses(User, Node)
%%           User = user record() | string(), username
%%           Node = atom(), the node we are to talk with
%% Descrip.: Fetch all addresses for a user. For legacy reasons,
%%           addresses are called numbers in the Mnesia backend.
%% Returns : list() of string()
%%--------------------------------------------------------------------
get_user_addresses(User, Node) when is_record(User, user), is_atom(Node) ->
    case get_user_addresses(User#user.user, Node) of
	[] ->
	    case User#user.number of
		L when is_list(L) ->
		    L;
		undefined ->
		    []
	    end;
	Res when is_list(Res) ->
	    Res
    end;
get_user_addresses(User, Node) when is_list(User), is_atom(Node) ->
    {atomic, L} = rpc:call(Node, phone, get_numbers_for_user, [User]),
    true = is_list(L),
    L.

%%--------------------------------------------------------------------
%% Function: fmt_expires(In)
%%           In = integer() | never
%% Descrip.: Format one of our 'expires' dates into a string telling
%%           how far in the future the 'expires' time is.
%% Returns : HTML = term(), Yaws html data
%%--------------------------------------------------------------------
fmt_expires(In) when is_integer(In) ->
    case In - util:timestamp() of
	N when is_integer(N), N >= 0 ->
	    Daystime = calendar:seconds_to_daystime(In - util:timestamp()),
	    fmt_daystime_short(Daystime);
	N when is_integer(N) ->
	    {strong, [], {font, [{color, red}], ["Expired"]}}
    end;
fmt_expires(never) ->
    "never".

%%--------------------------------------------------------------------
%% Function: fmt_daystime_short({D, {H, M, S}})
%%           D = integer(), days
%%           H = integer(), hours
%%           M = integer(), minutes
%%           S = integer(), seconds
%% Descrip.: Make a string out of a calendar modules "daystime".
%% Returns : string()
%%--------------------------------------------------------------------
fmt_daystime_short({0, {0, M, S}}) ->
    io_lib:format("~pm, ~ps", [M, S]);
fmt_daystime_short({0, {H, M, S}}) ->
    io_lib:format("~ph, ~pm, ~ps", [H, M, S]);
fmt_daystime_short({D, {H, M, S}}) ->
    io_lib:format("~pd, ~s", [D, fmt_daystime_short({0, {H, M, S}})]).



%%--------------------------------------------------------------------
%% Function: get_incomingproxy_node()
%% Descrip.: Get name of incomingproxy node. Per default we do this by
%%           figuring out our local hostname, and prepending it with
%%           "incomingproxy@". Patch this function if you want to run
%%           your web interface on another host than your
%%           incomingproxy (NOTE: you MUST make sure the nodes can
%%           talk to each other through distributed Erlang).
%% Returns : Nodename = string()
%%--------------------------------------------------------------------
get_yxa_application_node() ->
    {ok, MyHostname} = my_hostname(),
    "incomingproxy@" ++ MyHostname.

%% part of get_yxa_application_node/0, inet:gethostname/0 without the
%% domain-name removing part.
%% Returns : {ok, Hostname}, Hostname = string()
my_hostname() ->
    case inet_udp:open(0, []) of
        {ok, Socket} ->
            {ok, Hostname} = inet:gethostname(Socket),
            inet_udp:close(Socket),
	    case get_fqdn(Hostname) of
		{ok, FQDN} ->
		    {ok, FQDN};
		error ->
		    {ok, Hostname}
	    end;
        _ ->
            {ok, "nohost.nodomain"}
    end.

%%--------------------------------------------------------------------
%% Function: get_fqdn(H)
%%           H = string(), hostname
%% Descrip.: Get the FQDN (Fully Qualified Domain Name) for a
%%           (possibly) not fully qualified hostname.
%% Returns : {ok, FQDN} | error
%%           FQDN = string()
%%--------------------------------------------------------------------
get_fqdn(Hostname) ->
    case inet:gethostbyname(Hostname) of
	{ok, HostEnt} when is_record(HostEnt, hostent) ->
	    {ok, HostEnt#hostent.h_name};
	{error, What} ->
	    logger:log(debug, "Resolver: Error ~p when resolving local hostname (~s)", [What, Hostname]),
	    error
    end.
