%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Serialization support for converting a digraph graph to the <a
%%% href="http://www.graphviz.org/doc/info/lang.html">dot</a> file
%%% format.
%%% @end
%%%-------------------------------------------------------------------
-module(digraph_export_dot).
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
    Ids = ids(Graph),
    Vertices = [format_vertex(V, Ids, indent(Pretty)) ||
                V <- vertices(Graph)],
    Edges = [format_edge(E, Ids, indent(Pretty)) ||
             E <- edges(Graph)],
    io_lib:format("digraph ~ts{~n~ts~ts}~n",
                  [pad(Name), Vertices, Edges]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

pad("") ->
    "";
pad(String) ->
    [String, " "].

indent(true) ->
    "\t";
indent(false) ->
    "".

quoted(Term) ->
    String = io_lib:format("~tw", [Term]),
    % Order matters here.
    Slashes = string:replace(String, "\\", "\\\\"),
    Quotes = string:replace(Slashes, "\"", "\\\""),
    [$", Quotes, $"].

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

ids(Graph) ->
    Vertices = digraph:vertices(Graph),
    maps:from_list([{V, ["n_", integer_to_list(I)]} ||
                    {I, V} <- enumerate(Vertices)]).

vertices(Graph) ->
    [digraph:vertex(Graph, V) || V <- digraph:vertices(Graph)].

edges(Graph) ->
    [digraph:edge(Graph, E) || E <- digraph:edges(Graph)].

format_vertex({V, Label}, Ids, Indent) ->
    #{V := Id} = Ids,
    io_lib:format("~ts~ts [id=~ts, label=~ts];~n",
                  [Indent, Id, quoted(V), quoted(Label)]).

format_edge({Edge, V1, V2, Label}, Ids, Indent) ->
    #{V1 := Id1} = Ids,
    #{V2 := Id2} = Ids,
    io_lib:format("~ts~ts -> ~ts [id=~ts, label=~ts];~n",
                  [Indent, Id1, Id2, quoted(Edge), quoted(Label)]).
