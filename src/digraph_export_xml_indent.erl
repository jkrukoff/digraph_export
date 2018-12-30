%%%-------------------------------------------------------------------
%%% @private
%%% @doc
%%% Imperfect support for inserting human readable indentation into an
%%% xmerl XML document. Does not handle end tags properly.
%%%
%%% Intended to be used as an xmerl callback module for the export
%%% functions.
%%% @end
%%%-------------------------------------------------------------------
-module(digraph_export_xml_indent).

%% API
-export(['#xml-inheritance#'/0,
         '#element#'/5]).

%%%===================================================================
%%% API
%%%===================================================================

'#xml-inheritance#'() ->
    ['xmerl_xml'].

'#element#'(Tag, Data, Attrs, Parents, E) ->
    [indent(Parents),
     xmerl_xml:'#element#'(Tag, Data, Attrs, Parents, E)].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

indent(Parents) ->
    [io_lib:nl(), lists:duplicate(length(Parents), $	)].
