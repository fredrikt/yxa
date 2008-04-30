%%%-------------------------------------------------------------------
%%% File    : su_bot_refer.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Do REFER stuff for JEvent bot.
%%%
%%% @since     2 Mar 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(su_bot_refer).
%%-compile(export_all).


%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([start_link/4
	]).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([child/5
	]).


%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("siprecords.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
%% @type state() = #state{}.
%%                 no description
-record(state, {opaque,		%% term()
		referer,	%% sipurl record()
		referee,	%% sipurl record()
		refer_to	%% sipurl record()
	       }).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(TIMEOUT, 300).
-define(CMD, "REFER-REQUEST").

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    () -> term()
%%
%% @doc     Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Opaque, Referer, Referee, ReferTo) when is_record(Referer, sipurl), is_record(Referee, sipurl),
						   is_record(ReferTo, sipurl) ->
    Pid = spawn_link(?MODULE, child, [self(), Opaque, Referer, Referee, ReferTo]),
    logger:log(debug, "SU bot refer: Started child ~p to handle request : ~p",
	       [{opaque, Opaque},
		{referer, Referer},
		{referee, Referee},
		{refer_to, ReferTo}
	       ]),
    Pid.


child(Parent, Opaque, Referer, Referee, ReferTo)
  when is_pid(Parent), is_record(Referer, sipurl), is_record(Referee, sipurl),
       is_record(ReferTo, sipurl) ->
    %% we need to trap exits from our REFER workers
    process_flag(trap_exit, true),
    case refer:real_start(contact:new(Referee),
			  contact:new(ReferTo),
			  contact:new(Referer),
			  ?TIMEOUT, fun refer_logfun/3, self(), []) of
	{ok, Pid, CallId, LocalTag} ->
	    Res = loop(Pid, Parent, Opaque),
	    sipdialog:delete_dialog_controller(CallId, LocalTag, undefined),
	    Res;
	error ->
	    gen_server:cast(Parent, {send_to_port, self(), ?CMD, Opaque, "FAILED. Terminating."}),
	    error
    end.

loop(Pid, Parent, Opaque) ->
    receive
	{refer_progress, Pid, Method, Progress} ->
	    gen_server:cast(Parent, {send_to_port, self(), ?CMD " continued", Opaque,
				     ["Progress: ", Method, " ", Progress]}),
	    logger:log(debug, "SU bot refer: Method ~p progress ~p'", [Method, Progress]),
	    loop(Pid, Parent, Opaque);
	{'EXIT', Pid, normal} ->
	    gen_server:cast(Parent, {send_to_port, self(), ?CMD, Opaque, "Finished. Terminating."}),
	    logger:log(normal, "SU bot refer: ~p finished with exit status 'normal'", [Pid]),
	    ok;
	{'EXIT', Pid, Reason} ->
	    gen_server:cast(Parent, {send_to_port, self(), ?CMD, Opaque, "EXITED ABNORMALLY. Terminating."}),
	    logger:log(error, "SU bot refer: ~p finished with ABNORMAL exit status.", [Pid]),
	    logger:log(debug, "SU bot refer: Non-normal exit reason for pid ~p was :~n~p", [Pid, Reason]),
	    error
    end.

refer_logfun(Level, Format, Arguments) ->
    logger:log(Level, Format, Arguments).
