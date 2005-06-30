%% A minimalistic autotest suite for YXA. Test granularity is on a
%% module basis, rather than the prefered test cases basis.
%%
%%--------------------------------------------------------------------

-module(autotest).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 run/0,
	 run/1,
	 run_cover/1,
	 fail/1
	]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
	 fake_logger_loop/0
	]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

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
		       siptimer,
		       yxa_config_erlang,
		       yxa_config_check
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
	    io:format("Faking Yxa runtime environment...~n"),
	    %%mnesia:start(),
	    %%directory:start_link(),
	    Logger = spawn(?MODULE, fake_logger_loop, []),
	    register(logger, Logger),
	    
	    {ok, _CfgPid} = yxa_config:start_link({autotest, incomingproxy});
	_ -> ok
    end,
    
    {{Year,Month,Day},{Hour,Min,_Sec}} = calendar:local_time(),
    TimeStr = integer_to_list(Year) ++ "-" ++ string:right(integer_to_list(Month), 2, $0) ++ "-"
	++ string:right(integer_to_list(Day), 2, $0) ++ " " ++ string:right(integer_to_list(Hour), 2, $0)
	++ ":" ++ string:right(integer_to_list(Min), 2, $0),

    io:format("~n"),
    io:format("**********************************************************************~n"),
    io:format("*                      AUTOTEST STARTED            ~s  *~n",[TimeStr]),
    io:format("**********************************************************************~n"),
    Results = [{Module, test_module(Module)} || Module <- ?TEST_MODULES],


    io:format("~n"),
    io:format("======================================================================~n"),
    io:format("=                        TEST RESULTS                                =~n"),
    io:format("======================================================================~n"),

    put(autotest_result, ok),

    F = fun({Module,Res}) ->
		case Res of
		    ok ->
			io:format("~25w: passed~n", [Module]);
		    _ ->
			io:format("~25w: failed - Error:~n~p~n~n", [Module,Res]),
			put(autotest_result, error)
		end
	end,
    lists:foreach(F, Results),

    Status = erase(autotest_result),

    %% temp fix (?) to ensure that everything gets printed before halt/1 terminates erts
    timer:sleep(1000), % 1s

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
    case catch Module:test() of
	ok ->
	    ok;
	Error ->
	    io:format("Test FAILED : ~p~n", [Error]),
	    Error
    end.


%%--------------------------------------------------------------------
%% Function: run_cover([Mode])
%%           Mode = erl | shell
%% Descrip.: Run our tests after cover-compiling the modules. Show
%%           some numbers about the coverage ratio before exiting.
%% Returns : -
%%--------------------------------------------------------------------
run_cover([Mode]) ->
    io:format("Cover-compiling ~p modules : ", [length(?TEST_MODULES)]),

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
		end, 0, ?TEST_MODULES),
    io:format("~p~n", [length(?TEST_MODULES)]),

    %% Run the tests
    Status = run([erl]),

    %% Calculate coverage
    {ok, CoveredLines, TotalLines, ModuleStats} = aggregate_coverage(?TEST_MODULES),

    io:format("~n"),
    io:format("======================================================================~n"),
    io:format("=                        COVER RESULTS                               =~n"),
    io:format("======================================================================~n"),

    put(autotest_result, ok),

    F = fun({Module, Percent}) ->
		io:format("~25w: ~.1f%~n", [Module, Percent])
	end,
    lists:foreach(F, lists:reverse(
		       lists:keysort(2, ModuleStats)
		      )),

    TotalPercent = (CoveredLines / TotalLines * 100),

    io:format("~nTests covered ~p out of ~p lines of code in ~p modules (~p%)~n~n",
	      [CoveredLines, TotalLines, length(?TEST_MODULES), TotalPercent]),

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


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: fake_logger_loop()
%% Descrip.: Main loop for a process that does nothing more than
%%           registers itself as 'logger'. This is needed when this
%%           module is executed from a unix-shell, instead of from an
%%           erlang prompt with an Yxa application running.
%% Returns : does not return.
%%--------------------------------------------------------------------
fake_logger_loop() ->
    receive
	exit ->
	    ok;
	_ ->
	    fake_logger_loop()
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
    {ok, {_Module, {Cov, NotCov}}} = cover:analyse(H, coverage, module),
    ModLines = Cov + NotCov,
    ModPercent = (Cov / ModLines * 100),
    NewModuleStats = [{H, ModPercent} | ModuleStats],
    aggregate_coverage2(T,
			CoveredLines + Cov,
			TotalLines + ModLines,
			NewModuleStats
		       );
aggregate_coverage2([], CoveredLines, TotalLines, ModuleStats) ->
    {ok, CoveredLines, TotalLines, ModuleStats}.
