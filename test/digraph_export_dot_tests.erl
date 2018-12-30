%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(digraph_export_dot_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================


convert_test() ->
    Graph = digraph:new(),
    digraph:add_vertex(Graph, 1, first),
    digraph:add_vertex(Graph, 2, second),
    digraph:add_edge(Graph, edge, 1, 2, connected),

    ?assertEqual("digraph test {\n"
                 "\tn_1 [id=\"1\", label=\"first\"];\n"
                 "\tn_2 [id=\"2\", label=\"second\"];\n"
                 "\tn_1 -> n_2 [id=\"edge\", label=\"connected\"];\n"
                 "}\n",
                 unicode:characters_to_list(
                   digraph_export_dot:convert(Graph, "test", true))).
