%% A minimalistic autotest suite for YXA. Test granularity is on a
%% module basis, rather than the prefered test cases basis.
%%
%%--------------------------------------------------------------------

-module(autotest).
%%-compile(export_all).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 run/0,
	 run/1,
	 run_cover/1,
	 fail/1,
	 mark/2,
	 mark/3,

	 aggregate_coverage/1
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 test_module2/2,
	 fake_logger_loop/1
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("sipsocket.hrl").
-include("phone.hrl").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(TEST_MODULES, [
		       url_param,
		       contact_param,
		       keylist,
		       sipparse_util,
		       sipurl,
		       contact,
		       sipheader,
		       siprequest,
		       sippacket,
		       sipserver,
		       servertransaction,
		       clienttransaction,
		       sipdst,
		       xml_parse_util,
		       xml_parse,
		       interpret_backend,
		       interpret_cpl,
		       ts_date,
		       ts_datetime,
		       ts_duration,
		       interpret_time,
		       tcp_receiver,
		       targetlist,
		       util,
		       dnsutil,
		       siplocation,
		       lookup,
		       sipauth,
		       sipproxy,
		       sipuserdb_file_backend,
		       sipuserdb_file,
		       siptimer,
		       yxa_config_erlang,
		       yxa_config_check,
		       transportlayer,
		       sipsocket,
		       sipsocket_tcp,
		       tcp_connection,
		       ssl_util,
		       siphost,
		       hex,
		       sipdialog,
		       sipsocket_blacklist,
		       stun,
		       appserver,
		       presence_pidf
		      ]).

%%====================================================================
%% External functions
%%====================================================================


%%--------------------------------------------------------------------
%% Function: run([Mode])
%%           run()
%%           Mode = erl | shell
%% Descrip.: Test all modules listed in ModulesToAutoTest and print
%%           the result. If Mode is 'shell' we end with halting the
%%           erlang runtime system, with exit status 1 if any tests
%%           fail and 0 if they are all successfull.
%% Returns : -
%%--------------------------------------------------------------------
run() ->
    run([erl]).

run([Mode]) ->
    case erlang:whereis(logger) of
	undefined ->
	    io:format("Faking YXA runtime environment...~n"),
	    %%mnesia:start(),
	    %%directory:start_link(),
	    Logger = spawn(?MODULE, fake_logger_loop, [false]),
	    register(logger, Logger),

	    {ok, _CfgPid} = yxa_config:start_link({autotest, incomingproxy}),

	    ets:new(yxa_sipsocket_info, [public, bag, named_table]),
	    ets:new(yxa_sipsocket_blacklist, [public, bag, named_table]),
	    ets:insert(yxa_sipsocket_info, {self(), #yxa_sipsocket_info_e{proto = tcp,
									  addr  = "0.0.0.0",
									  port  = 5060
									 }}
		      ),
	    ets:insert(yxa_sipsocket_info, {self(), #yxa_sipsocket_info_e{proto = udp,
									  addr  = "0.0.0.0",
									  port  = 5060
									 }}
		       ),
	    ets:insert(yxa_sipsocket_info, {self(), #yxa_sipsocket_info_e{proto = tls,
									  addr  = "0.0.0.0",
									  port  = 5061
									 }}
		      ),

	    ets:new(yxa_hooks, [named_table, set]),


	    case mnesia:system_info(is_running) of
		yes ->
		    ok;
		no ->
		    io:format("Starting Mnesia..."),
		    ok = mnesia:start(),
		    %erlang:put(test_started_mnesia, true),
		    io:format("ok~n")
	    end;
	_ ->
	    ok
    end,

    {{Year,Month,Day},{Hour,Min,_Sec}} = calendar:local_time(),
    TimeStr = integer_to_list(Year) ++ "-" ++ string:right(integer_to_list(Month), 2, $0) ++ "-"
	++ string:right(integer_to_list(Day), 2, $0) ++ " " ++ string:right(integer_to_list(Hour), 2, $0)
	++ ":" ++ string:right(integer_to_list(Min), 2, $0),

    io:format("~n"),
    io:format("**********************************************************************~n"),
    io:format("*                      AUTOTEST STARTED            ~s  *~n",[TimeStr]),
    io:format("**********************************************************************~n"),
    Results = [{Module, test_module(Module)} || Module <- get_test_modules()],


    io:format("~n"),
    io:format("======================================================================~n"),
    io:format("=                        TEST RESULTS                                =~n"),
    io:format("======================================================================~n"),

    put(autotest_result, ok),

    F = fun({Module,Res}) ->
		case Res of
		    ok ->
			io:format("~25w: passed~n", [Module]);
		    {Line, Name, Error} ->
			io:format("~25w: failed (test ~p, line ~p) - Error:~n~p~n~n",
				  [Module, lists:flatten(Name), Line, Error]),
			put(autotest_result, error)
		end
	end,
    lists:foreach(F, Results),

    Status = erase(autotest_result),

    %% temp fix (?) to ensure that everything gets printed before halt/1 terminates erts
    timer:sleep(1000), % 1s

%    case erlang:erase(test_started_mnesia) of
%	true ->
%	    io:format("~nStopping Mnesia since I started it..."),
%	    stopped = mnesia:stop(),
%	    io:format("ok~n");
%	_ ->
%	    ok
%    end,
    
    if
	Status == error, Mode == shell -> erlang:halt(1);
	Status == ok, Mode == shell -> erlang:halt(0);
	true ->
	    Status
    end.

%%--------------------------------------------------------------------
%% Function: test_module(Module)
%%           Module = atom(), a module name
%% Descrip.: test a single module
%% Returns : ok | ERROR
%%
%% The Module:test() function that must be supplied by the module
%% Module:
%%
%% Function: test()
%% Descrip.: test function for this module
%% Returns : ok (if all works as expected) | throw() (if there is some
%%           kind of error in the code or test code)
%% Note    : each test in the test function should print a single line
%%           "test: TestedFunction/Arity - TestNo_For_TestedFunction"
%%           + "~n". It's mainly used to make it easier to find the
%%           failing sub test.
%% Note    : individual subtest in test() should preferably be
%%           independent of each other (test setup and execution
%%           order) - both for readability and if more advanced
%%           autotest systems are implemented
%%--------------------------------------------------------------------
test_module(Module) ->
    io:format("~n"),
    io:format("testing module: ~p~n",[Module]),
    io:format("----------------------------------------------------------------------~n"),
    %% run each module test in a new pid to avoid being affected by other tests (like
    %% having signals in the process mailbox, getting old timers firing upon us etc.)
    TestPid = spawn_link(?MODULE, test_module2, [self(), Module]),
    receive
	{test_result, TestPid, Res} ->
	    case Res of
		ok ->
		    ok;
		{autotest_error, Line, Name, Error} ->
		    io:format("Test FAILED : ~p~n", [Error]),
		    {Line, Name, Error}
	    end
    after 600 * 1000 ->
	    %% just to avoid hangs in case the execution of the tests are automated
	    %% and runs from cron or similar
	    io:format("Test FAILED : never finished~n"),
	    {0, "Unknown", "module test never finished"}
    end.

%% test_module2 - part of test_module but runs in a separate pid and
%% reports result to parent (test_module)
test_module2(Parent, Module) ->
    Res =
	case catch Module:test() of
	    ok ->
		fail_on_leftover_messages();
	    Error ->
		{Line, Name} = erase({autotest, position}),
		{autotest_error, Line, Name, Error}
	end,
    Parent ! {test_result, self(), Res}.

fail_on_leftover_messages() ->
    fail_on_leftover_messages([]).
fail_on_leftover_messages(Res) when is_list(Res) ->
    receive
	M ->
	    fail_on_leftover_messages([M | Res])
    after 0 ->
	    %% no (more) messages in mailbox
	    case Res of
		[] -> ok;
		_ ->
		    Msg = ["The following ",
			   case length(Res) of
			       1 -> "message";
			       Len -> lists:concat([Len, " messages"])
			   end,
			   " was in the test processes mailbox when it finished :"
			  ],
		    {autotest_error, 0, "Messages left in process mailbox",
		      [lists:flatten(Msg), lists:reverse(Res)
		      ]
		     }
	    end
    end.

%%--------------------------------------------------------------------
%% Function: run_cover([Mode])
%%           Mode = erl | shell
%% Descrip.: Run our tests after cover-compiling the modules. Show
%%           some numbers about the coverage ratio before exiting.
%% Returns : -
%%--------------------------------------------------------------------
run_cover([Mode]) ->
    TestModules = get_test_modules(),

    io:format("Cover-compiling ~p modules : ", [length(TestModules)]),

    %% cover-compile all our test modules
    lists:foldl(fun(Module, Num) ->
			cover:compile_beam(Module),
			case Num rem 5 of
			    0 ->
				io:format("~p", [Num]);
			    _ ->
				io:format(".")
			end,
			Num + 1
		end, 0, TestModules),
    io:format("~p~n", [length(TestModules)]),

    %% Run the tests
    Status = run([erl]),

    %% Calculate coverage
    {ok, CoveredLines, TotalLines, ModuleStats} = aggregate_coverage(TestModules),

    io:format("~n"),
    io:format("======================================================================~n"),
    io:format("=                        COVER RESULTS                               =~n"),
    io:format("======================================================================~n"),

    put(autotest_result, ok),

    F = fun({Module, Percent, _Covered, _NotCovered}) ->
		io:format("~25w: ~.1f%~n", [Module, Percent])
	end,
    lists:foreach(F, lists:reverse(
		       lists:keysort(2, ModuleStats)
		      )),

    TotalPercent = (CoveredLines / TotalLines * 100),

    io:format("~nTests covered ~p out of ~p lines of code in ~p modules (~.1f%)~n~n",
	      [CoveredLines, TotalLines, length(TestModules), TotalPercent]),

    if
	Status == error, Mode == shell -> erlang:halt(1);
	Status == ok, Mode == shell -> erlang:halt(0);
	true ->
	    Status
    end.

%%--------------------------------------------------------------------
%% Function: fail(Fun)
%%           Fun = fun()
%% Descrip.: test case support function, used to check if call Fun()
%%           fails - as expected
%% Returns : ok | throw() (if Fun did not generate a exception)
%%--------------------------------------------------------------------
fail(Fun) ->
    try Fun() of
	_  -> throw({error, no_exception_thrown_by_test})
    catch
	_ -> ok %% catch user throw()
    end.


%%--------------------------------------------------------------------
%% Function: mark(Line, Msg)
%%           mark(Line, Fmt, Args)
%%           Line = integer() | undefined
%%           Fmt  = string()
%%           Args = list()
%% Descrip.: Mark a new test. Record whereabout information in the
%%           process dictionary to help locate failing tests at the
%%           end result stage.
%% Returns : ok
%%--------------------------------------------------------------------
mark(Line, Msg) ->
    mark(Line, Msg, []).

mark(Line, Fmt, Args) when is_list(Fmt), is_list(Args) ->
    Name = io_lib:format(Fmt, Args),
    io:put_chars(["test: ", Name, "\n"]),
    put({autotest, position}, {Line, Name}),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: fake_logger_loop(Enabled)
%%           Enabled = bool(), output to console or not?
%% Descrip.: Main loop for a process that does nothing more than
%%           registers itself as 'logger'. This is needed when this
%%           module is executed from a unix-shell, instead of from an
%%           erlang prompt with an YXA application running.
%% Returns : does not return.
%%--------------------------------------------------------------------
fake_logger_loop(Enabled) ->
    receive
	exit ->
	    ok;
	{'$gen_cast', {log, _Level, Data}} when Enabled == true ->
	    io:format("~s~n", [binary_to_list(Data)]),
	    fake_logger_loop(Enabled);
	enable ->
	    fake_logger_loop(true);
	disable ->
	    fake_logger_loop(false);
	_ ->
	    fake_logger_loop(Enabled)
    end.

%%--------------------------------------------------------------------
%% Function: aggregate_coverage(Modules)
%%           Modules = list() of atom(), list of module names
%% Descrip.: Aggregate cover analysis data for Modules.
%% Returns : {ok, CoveredLines, TotalLines, ModuleStats}
%%           CoveredLines = integer(), code lines covered in Modules
%%           TotalLines   = integer(), total lines of code in Modules
%%           ModuleStats  = list() of {Module, Percent} tuple()
%%--------------------------------------------------------------------
aggregate_coverage(Modules) ->
    aggregate_coverage2(Modules, 0, 0, []).

aggregate_coverage2([H | T], CoveredLines, TotalLines, ModuleStats) ->
   %% {ok, {_Module, {Cov, NotCov}}} = cover:analyse(H, coverage, module),
    {ok, Cov, NotCov} = get_module_coverage(H),
    ModLines = Cov + NotCov,
    ModPercent = (Cov / ModLines * 100),
    This = {H, ModPercent, Cov, NotCov},
    NewModuleStats = [This | ModuleStats],
    aggregate_coverage2(T,
			CoveredLines + Cov,
			TotalLines + ModLines,
			NewModuleStats
		       );
aggregate_coverage2([], CoveredLines, TotalLines, ModuleStats) ->
    {ok, CoveredLines, TotalLines, ModuleStats}.

%%--------------------------------------------------------------------
%% Function: get_module_coverage(Module)
%%           Module = atom(), module name
%% Descrip.: Get number of covered/not covered lines of real code in
%%           a module. Excludes all functions named test* from the
%%           bottom of the coverage data for Module.
%% Returns : {ok, Cov, NotCov}
%%           Cov    = integer(), code lines covered in Module
%%           NotCov = integer(), lines not covered
%%--------------------------------------------------------------------
get_module_coverage(Module) when is_atom(Module) ->
    {ok, Data} = cover:analyse(Module, coverage, function),
    {ok, Data2} = remove_test_functions_data(Data),
    get_module_coverage2(Data2, 0, 0).

get_module_coverage2([{_MFA, {Cov, NotCov}} | T], TotCov, TotNotCov) ->
    get_module_coverage2(T, TotCov + Cov, TotNotCov + NotCov);
get_module_coverage2([], TotCov, TotNotCov) ->
    {ok, TotCov, TotNotCov}.

remove_test_functions_data(Data) when is_list(Data) ->
    In = lists:reverse(Data),
    Out = remove_test_functions_data2(In),
    {ok, lists:reverse(Out)}.

%% Remove all entrys for functions named test* until we find
%% one that does not match. Should be fed a modules coverage data
%% in reverse order since the test* functions are at the bottom.
remove_test_functions_data2([{{_M, F, _A}, {_Cov, _NotCov}} = H | T]) ->
    case atom_to_list(F) of
	"test" ++ _ ->
	    remove_test_functions_data2(T);
	_ ->
	    [H | T]
    end;
remove_test_functions_data2([]) ->
    [].


get_test_modules() ->
    case os:getenv("YXA_AUTOTEST_MODULES") of
	Env when is_list(Env) ->
	    [list_to_atom(Mod) || Mod <- string:tokens(Env, " ")];
	false ->
	    ?TEST_MODULES
    end.
