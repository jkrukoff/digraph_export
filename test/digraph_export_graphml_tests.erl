%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(digraph_export_graphml_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

convert_test() ->
    Graph = digraph:new(),
    digraph:add_vertex(Graph, 1, first),
    digraph:add_vertex(Graph, 2, second),
    digraph:add_edge(Graph, edge, 1, 2, connected),

    ?assertEqual("<?xml version=\"1.0\"?>"
                 "\n<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" "
                     "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" "
                     "xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns "
                     "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">"
                 "\n\t<key id=\"label\" for=\"all\" attr.name=\"label\" attr.type=\"string\"/>"
                 "\n\t<graph id=\"test\" edgedefault=\"directed\">"
                 "\n\t\t<node id=\"1\">"
                 "\n\t\t\t<data key=\"label\">first</data></node>"
                 "\n\t\t<node id=\"2\">"
                 "\n\t\t\t<data key=\"label\">second</data></node>"
                 "\n\t\t<edge id=\"edge\" source=\"1\" target=\"2\">"
                 "\n\t\t\t<data key=\"label\">connected</data></edge></graph></graphml>",
                 unicode:characters_to_list(
                   digraph_export_graphml:convert(Graph, "test", true))).
