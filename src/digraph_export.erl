%%%-------------------------------------------------------------------
%%% @doc
%%% Utility functions for converting and viewing digraph graphs.
%%% @end
%%%-------------------------------------------------------------------
-module(digraph_export).

-define(TEMP_LABEL, (atom_to_list(?MODULE))).
-define(TEMP_FILE, "graph").

%% API
-export([formats/0,
         programs/0,
         convert/2,
         convert/3,
         view/3]).

-type format() :: dot | graphml.
%% Available output formats.
-type formats() :: [format(), ...].
%% A list of output formats.
-type program() :: cytoscape | gephi.
%% Available graph viewer programs.
-type programs() :: [program(), ...].
%% A list of graph viewer programs.

-type convert_properties() :: [{name, string()} |
                               pretty | 
                               {pretty, boolean()}].
%% Conversion proplist values.

-export_type([format/0,
              formats/0,
              program/0,
              programs/0,
              convert_properties/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% List all file formats supported by the convert functions.
%% @end
%% @see convert/2
%% @see convert/3
-spec formats() -> Formats when
      Formats :: formats().
formats() ->
    [dot, graphml].

%% @doc
%% List all external viewing programs supported by the view function.
%% @end
%% @see view/3
-spec programs() -> Programs when
      Programs :: programs().
programs() ->
    [cytoscape, gephi].

%% @equiv convert/3
-spec convert(Graph, Format) -> Serialized when
      Graph :: digraph:graph(),
      Format :: format(),
      Serialized :: unicode:charlist().
convert(Graph, Format) ->
    convert(Graph, Format, []).

%% @doc
%% Serialize a digraph graph to the given file format.
%%
%% Options are passed as a property list. The two supported options
%% are:
%%
%% <ul>
%% <li>Name: An optional name to include in the graph file.</li>
%% <li>Pretty: A boolean value for if the output file should be
%% formatted for human readability or optimized for size.</li>
%% </ul>
%% @end
%% @param Graph An existing digraph graph.
%% @param Format One of the supported file formats.
%% @param Options Property list of conversion options.
%% @returns The serialized graph data.
%% @see formats/0
-spec convert(Graph, Format, Options) -> Serialized when
      Graph :: digraph:graph(),
      Format :: format(),
      Options :: convert_properties(),
      Serialized :: unicode:charlist().
convert(Graph, dot, Options) ->
    {Name, Pretty} = parse_convert_properties(Options),
    digraph_export_dot:convert(Graph, Name, Pretty);
convert(Graph, graphml, Options) ->
    {Name, Pretty} = parse_convert_properties(Options),
    digraph_export_graphml:convert(Graph, Name, Pretty).

%% @doc
%% Launch an external program to view a serialized graph. A temporary
%% file is created to store the graph and passed into the external
%% program.
%%
%% The external program will need to be installed and on the current
%% PATH for this to function.
%%
%% This will block on the external program completing. Please spawn
%% this function in a separate process if that is not desired.
%% @end
%% @param Converted The serialized graph data.
%% @param Format The format the graph was serialized in.
%% @param Program The external program to launch.
%% @returns The output of the program.
%% @see formats/0
%% @see programs/0
-spec view(Serialized, Format, Program) -> Output when
      Serialized :: unicode:charlist(),
      Format :: format(),
      Program :: program(),
      Output :: string().
view(Serialized, Format, Program) ->
    Data = unicode:characters_to_binary(Serialized),
    {ok, TempFile} = mktemp(?TEMP_LABEL, ?TEMP_FILE ++ extension(Format)),
    try
        ok = file:write_file(TempFile, Data),
        Command = io_lib:format(command(Program), [TempFile]),
        _ = os:cmd(Command)
    after
        ok = file:delete(TempFile)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

parse_convert_properties(Options) when is_list(Options) ->
    Name = proplists:get_value(name, Options, ""),
    Pretty = proplists:get_bool(indent, Options),
    {Name, Pretty}.

extension(dot) -> ".dot";
extension(graphml) -> ".graphml".

command(gephi) -> "gephi --console suppress \"~ts\"";
command(cytoscape) -> "cytoscape -N \"~ts\"".

-spec mktemp(Label, File) -> {ok, TempFile} | {error, Reason} when
      Label :: string(),
      File :: string(),
      TempFile :: file:filename(),
      Reason :: file:posix().
mktemp(Label, File) ->
    RandBytes = crypto:strong_rand_bytes(8),
    RandChars = integer_to_list(binary:decode_unsigned(RandBytes), 36),
    TempDir = filename:basedir(user_cache, Label ++ "-" ++ RandChars),
    TempFile = filename:join(TempDir, File),
    EnsureResult = filelib:ensure_dir(TempFile),
    WriteResult = file:write_file(TempFile, <<>>),
    case {EnsureResult, WriteResult} of
         {ok, ok}    -> {ok, TempFile};
         {ok, Error} -> Error;
         {Error, _}  -> Error
    end.
