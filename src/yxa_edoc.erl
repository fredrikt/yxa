%%%-------------------------------------------------------------------
%%% File    : yxa_edoc.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Run edoc with YXA parameters.
%%%
%%% Created : 15 Dec 2006 by Fredrik Thulin <ft@it.su.se>
%%%-------------------------------------------------------------------
-module(yxa_edoc).

-export([run/1
	]).


%%--------------------------------------------------------------------
%% Function: run(Args)
%%           Args = list() of string(), the first entry is our list
%%                  of files.
%% Descrip.: Run edoc on a number of files, with a constructed set
%%           of options.
%% Returns : ok
%%--------------------------------------------------------------------
run(Args) ->
    FilesIn = string:tokens(hd(Args), " "),

    {ok, Options, Files} = get_options(FilesIn),

    io:format("~n~n~nRunning edoc,~n~n  options : ~p~n~n  files   : ~p~n~n~n", [Options, Files]),

    ok = edoc:files(Files, Options),
    ok.

%%--------------------------------------------------------------------
%% Function: get_options(Filelist)
%%           Filelist = list() of string(), filenames we are to run
%%                      edoc on. Might need path prepending.
%% Descrip.: Look if any of the following command line options were
%%           passed to the emulator, and construct an edoc Options
%%           parameter out of them :
%%
%%             yxa_edoc_dir - specifies output directory
%%             yxa_edoc_gen - if the value of this parameter is
%%                            "private" then we set {private, true}
%%             yxa_edoc_srcpath - the source directory for the files
%%                                in Filelist
%% Returns : {ok, Options, Files}
%%           Options = list() of {Key, Value}
%%             Key   = atom()
%%             Value = term()
%%           Files   = list() of string()
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
