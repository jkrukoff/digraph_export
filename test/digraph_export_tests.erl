%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(digraph_export_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

formats_test() ->
    ?assert(length(digraph_export:formats()) > 0).

convert_test_() ->
    Graph = digraph:new(),
    digraph:add_vertex(Graph),
    digraph:add_vertex(Graph, node1),
    digraph:add_vertex(Graph, node2, label),
    digraph:add_edge(Graph, node1, node2),
    digraph:add_edge(Graph, node1, node2, label),
    digraph:add_edge(Graph, edge, node1, node2, label),

    [{lists:flatten(io_lib:format("convert graph to ~ts format", [F])),
      ?_assert(length(digraph_export:convert(Graph, F)) > 0)} ||
     F <- digraph_export:formats()].
