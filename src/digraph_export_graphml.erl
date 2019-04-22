%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Serialization support for converting a digraph graph to the <a
%%% href="http://graphml.graphdrawing.org/">GraphML</a> format.
%%%
%%% This is an XML based format.
%%% @end
%%%-------------------------------------------------------------------
-module(digraph_export_graphml).
-behaviour(digraph_exporter).

%% API
-export([convert/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec convert(Graph, Name, Pretty) -> Serialized when
      Graph :: digraph:graph(),
      Name :: unicode:charlist(),
      Pretty :: boolean(),
      Serialized :: unicode:charlist().
convert(Graph, Name, Pretty) ->
    Nodes = [format_vertex(V) || V <- vertices(Graph)],
    Edges = [format_edge(E) || E <- edges(Graph)],
    Root = graphml_element([graph_element(Name, Nodes ++ Edges)]),
    xmerl:export_simple([Root], exporter(Pretty)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

exporter(true) -> digraph_export_xml_indent;
exporter(false) -> xmerl_xml.

to_string(Term) ->
    lists:flatten(
      io_lib:format("~tw", [Term])).

graphml_element(Content) ->
    {graphml,
     [{xmlns, "http://graphml.graphdrawing.org/xmlns"},
      {'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"},
      {'xsi:schemaLocation', "http://graphml.graphdrawing.org/xmlns "
                             "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd"}],
     [{key,
       [{id, "label"},
        {for, "all"},
        {'attr.name', "label"},
        {'attr.type', "string"}],
       []}] ++ Content}.

graph_element("", Content) ->
    {graph,
     [{edgedefault, "directed"}],
     Content};
graph_element(Id, Content) ->
    {graph,
     [{id, Id},
      {edgedefault, "directed"}],
     Content}.

label_element(Label) ->
    {data,
     [{key, "label"}],
     [to_string(Label)]}.

vertices(Graph) ->
    [digraph:vertex(Graph, V) || V <- digraph:vertices(Graph)].

edges(Graph) ->
    [digraph:edge(Graph, E) || E <- digraph:edges(Graph)].

format_vertex({V, Label}) ->
    {node,
     [{id, to_string(V)}],
     [label_element(Label)]}.

format_edge({Edge, V1, V2, Label}) ->
    {edge,
     [{id, to_string(Edge)},
      {source, to_string(V1)},
      {target, to_string(V2)}],
     [label_element(Label)]}.
