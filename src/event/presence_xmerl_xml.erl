%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.erlang.org/license/EPL1_0.txt
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is xmerl-0.6
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%



%%%-------------------------------------------------------------------
%%% File    : presence_xmerl_xml.erl
%%% @author   Fredrik Thulin <ft@it.su.se>
%%% @doc      xmerl callback module identical to xmerl_xml except that
%%%           we include the complete XML namespace in any presence
%%%           tuple that we output.
%%%
%%%           Originally xmerl_xml.erl from Erlang/OTP R11B-0.
%%%
%%% @since    19 Jun 2006 by Fredrik Thulin <ft@it.su.se>
%%% @end
%%% @private
%%%-------------------------------------------------------------------
-module(presence_xmerl_xml).

-export(['#xml-inheritance#'/0]).

-export(['#root#'/4,
	 '#element#'/5,
	 '#text#'/1]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("xmerl/include/xmerl.hrl").


%%====================================================================
%% External functions
%%====================================================================

'#xml-inheritance#'() ->
    xmerl_xml:'#xml-inheritance#'().

%% The '#text#' function is called for every text segment.
'#text#'(Text) ->
    xmerl_xml:'#text#'(Text).

'#root#'(Data, Attrs, Something, E) ->
    xmerl_xml:'#root#'(Data, Attrs, Something, E).

%% The '#element#' function is the default handler for XML elements.
'#element#'(Tag, [], Attrs, Parents, E) ->
    xmerl_xml:'#element#'(Tag, [], Attrs, Parents, E);
'#element#'(Tag, Data, Attrs, Parents, E) ->
    %% If this is a presence tuple node, we call fix_xml_ns/2
    NewAttrs =
	case E#xmlElement.expanded_name of
	    {'urn:ietf:params:xml:ns:pidf', tuple} ->
		fix_xml_ns(E, Attrs);
	    {'urn:ietf:params:xml:ns:cpim-pidf', tuple} ->
		fix_xml_ns(E, Attrs);
	    _ ->
		Attrs
	end,
    xmerl_xml:'#element#'(Tag, Data, NewAttrs, Parents, E).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec    (E, Attrs) ->
%%            NewAttrs
%%
%%            E     = #xmlElement{}
%%            Attrs = [#xmlAttribute{}]
%%
%%            NewAttrs = [#xmlAttribute{}]
%%
%% @doc     Add the complete XML namespace to Attrs.
%% @end
%%--------------------------------------------------------------------
fix_xml_ns(E, Attrs) ->
    Nodes = (E#xmlElement.namespace)#xmlNamespace.nodes,
    NS = [#xmlAttribute{name  = list_to_atom("xmlns:" ++ Key),
			value = atom_to_list(Val)
		       } || {Key, Val} <- Nodes],
    NS ++ Attrs.
