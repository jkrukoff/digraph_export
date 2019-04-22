%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Behaviour for graph file format conversion modules.
%%% @end
%%%-------------------------------------------------------------------
-module(digraph_exporter).

-callback convert(Graph, Name, Pretty) -> Serialized when
      Graph :: digraph:graph(),
      Name :: unicode:charlist(),
      Pretty :: boolean(),
      Serialized :: unicode:charlist().
