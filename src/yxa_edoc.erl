%%%-------------------------------------------------------------------
%%% File    : yxa_edoc.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      Run edoc with YXA parameters.
%%%
%%% @since    15 Dec 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%%-------------------------------------------------------------------
-module(yxa_edoc).

-export([run/1
	]).


%%--------------------------------------------------------------------
%% @spec    (Args) -> ok
%%
%%            Args = [string()] "the first entry is our list of files."
%%
%% @doc     Run edoc on a number of files, with a constructed set of
%%          options.
%% @end
%%--------------------------------------------------------------------
run(Args) ->
    FilesIn = string:tokens(hd(Args), " "),

    {ok, Options, Files} = get_options(FilesIn),

    io:format("~n~n~nRunning edoc,~n~n  options : ~p~n~n  files   : ~p~n~n~n", [Options, Files]),

    ok = edoc:files(Files, Options),
    ok.

%%--------------------------------------------------------------------
%% @spec    (Filelist) ->
%%            {ok, Options, Files}
%%
%%            Filelist = [string()] "filenames we are to run edoc on. Might need path prepending."
%%
%%            Options = [{Key, Value}]
%%            Key     = atom()
%%            Value   = term()
%%            Files   = [string()]
%%
%% @doc     Look if any of the following command line options were
%%          passed to the emulator, and construct an edoc Options
%%          parameter out of them :
%%          yxa_edoc_dir - specifies output directory yxa_edoc_gen -
%%          if the value of this parameter is "private" then we set
%%          {private, true} yxa_edoc_srcpath - the source directory
%%          for the files in Filelist
%% @end
%%--------------------------------------------------------------------
get_options(FileList) ->
    Options1 =
	case init:get_argument(yxa_edoc_dir) of
	    {ok, [Dir]} when is_list(Dir) ->
		[{dir, Dir}];
	    error ->
		throw('No yxa_edoc_dir supplied')
	end,

    Options2 =
	case init:get_argument(yxa_edoc_gen) of
	    {ok, ["private"]} ->
		[{private, true} | Options1];
	    {ok, _} ->
		throw('Bad yxa_edoc_gen value');
	    error ->
		Options1
	end,

    Options = Options2,

    Files =
	case init:get_argument(yxa_edoc_srcpath) of
	    {ok, [Path]} when is_list(Path) ->
		%% have to prefix each entry in FileList with Path
		[filename:join(Path, File) || File <- FileList];
	    {ok, _} ->
		throw('Bad yxa_edoc_srcpath value');
	    error ->
		FileList
	end,

    {ok, Options, Files}.
