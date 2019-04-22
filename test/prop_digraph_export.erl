%%%-------------------------------------------------------------------
%%% @doc
%%% Property tests for digraph conversion.
%%% @end
%%%-------------------------------------------------------------------
-module(prop_digraph_export).

-include_lib("proper/include/proper.hrl").

%% Property options.
-export([]).

%%%===================================================================
%%% Tests
%%%===================================================================

prop_convert() ->
    ?FORALL({Format, Options, Graph},
            {format(), options(), graph()},
            begin
                Serialized = digraph_export:convert(Graph, Format, Options),
                true = digraph:delete(Graph),
                is_list(Serialized)
            end).

%%%===================================================================
%%% Generators
%%%===================================================================

maybe(Type) ->
    union([[], [Type]]).

format() ->
    union(digraph_export:formats()).

options() ->
    ?LET({Name, Pretty},
         {maybe({name, string()}), maybe({pretty, boolean()})},
         Name ++ Pretty).

graph() ->
    ?LET({VertexArgs, EdgeArgs},
         ?LET(VertexArgs,
              vertex_args(),
              case length(VertexArgs) of
                  0 ->
                      {VertexArgs, []};
                  VertexCount ->
                      {VertexArgs, edge_args(VertexCount)}
              end),
         begin
             Graph = digraph:new(),
             ok = lists:foreach(
                    fun (Args) ->
                            erlang:apply(digraph, add_vertex, [Graph | Args])
                    end,
                    VertexArgs),
             ok = lists:foreach(
                    fun (Args) ->
                            WithVertices = lookup_args(Graph, Args),
                            erlang:apply(digraph,
                                         add_edge,
                                         [Graph | WithVertices])
                    end,
                    EdgeArgs),
             Graph
         end).

vertex_args() ->
    list(vertex_arg()).

vertex_arg() ->
    union([[], [any()], [any(), any()]]).

edge_args(VertexCount) ->
    list(edge_arg(VertexCount)).

edge_arg(VertexCount) ->
    ?LET({Start, End},
         {integer(1, VertexCount), integer(1, VertexCount)},
         union([[Start, End],
                [Start, End, any()],
                [any(), Start, End, any()]])).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

lookup_vertex(Graph, N) ->
    Vertices = digraph:vertices(Graph),
    if
        N > length(Vertices) ->
            % Sometimes duplicates will cause us to have less vertices
            % than expected, but always at least 1.
            lists:nth(1, Vertices);
        N =< length(Vertices) ->
            lists:nth(N, Vertices)
    end.

lookup_args(Graph, [Start, End]) ->
    [lookup_vertex(Graph, Start),
     lookup_vertex(Graph, End)];
lookup_args(Graph, [Start, End, Label]) ->
    [lookup_vertex(Graph, Start),
     lookup_vertex(Graph, End),
     Label];
lookup_args(Graph, [Edge, Start, End, Label]) ->
    [Edge,
     lookup_vertex(Graph, Start),
     lookup_vertex(Graph, End),
     Label].
