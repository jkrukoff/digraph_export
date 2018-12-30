

# Module digraph_export #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Utility functions for converting and viewing digraph graphs.

<a name="types"></a>

## Data Types ##




### <a name="type-convert_properties">convert_properties()</a> ###


<pre><code>
convert_properties() = [{name, string()} | pretty | {pretty, boolean()}]
</code></pre>




### <a name="type-format">format()</a> ###


<pre><code>
format() = dot | graphml
</code></pre>




### <a name="type-program">program()</a> ###


<pre><code>
program() = cytoscape | gephi
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#convert-2">convert/2</a></td><td>Equivalent to <tt>convert / 3</tt>.</td></tr><tr><td valign="top"><a href="#convert-3">convert/3</a></td><td>
Serialize a digraph graph to the given file format.</td></tr><tr><td valign="top"><a href="#formats-0">formats/0</a></td><td>
List all file formats supported by the convert functions.</td></tr><tr><td valign="top"><a href="#programs-0">programs/0</a></td><td>
List all external viewing programs supported by the view function.</td></tr><tr><td valign="top"><a href="#view-3">view/3</a></td><td>
Launch an external program to view a serialized graph.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="convert-2"></a>

### convert/2 ###

<pre><code>
convert(Graph::<a href="digraph.md#type-graph">digraph:graph()</a>, Format::<a href="#type-format">format()</a>) -&gt; <a href="unicode.md#type-charlist">unicode:charlist()</a>
</code></pre>
<br />

Equivalent to `convert / 3`.

<a name="convert-3"></a>

### convert/3 ###

<pre><code>
convert(Graph::<a href="digraph.md#type-graph">digraph:graph()</a>, Format::<a href="#type-format">format()</a>, Options::<a href="#type-convert_properties">convert_properties()</a>) -&gt; <a href="unicode.md#type-charlist">unicode:charlist()</a>
</code></pre>
<br />

`Graph`: An existing digraph graph.<br />`Format`: One of the supported file formats.<br />`Options`: Property list of conversion options.<br />

returns: The serialized graph data.

Serialize a digraph graph to the given file format.

Options are passed as a property list. The two supported options
are:

* Name: An optional name to include in the graph file.

* Pretty: A boolean value for if the output file should be
formatted for human readability or optimized for size.


__See also:__ [formats/0](#formats-0).

<a name="formats-0"></a>

### formats/0 ###

<pre><code>
formats() -&gt; [<a href="#type-format">format()</a>, ...]
</code></pre>
<br />

List all file formats supported by the convert functions.

__See also:__ [convert/2](#convert-2), [convert/3](#convert-3).

<a name="programs-0"></a>

### programs/0 ###

<pre><code>
programs() -&gt; [<a href="#type-program">program()</a>, ...]
</code></pre>
<br />

List all external viewing programs supported by the view function.

__See also:__ [view/3](#view-3).

<a name="view-3"></a>

### view/3 ###

<pre><code>
view(Converted::<a href="unicode.md#type-charlist">unicode:charlist()</a>, Format::<a href="#type-format">format()</a>, Program::<a href="#type-program">program()</a>) -&gt; string()
</code></pre>
<br />

`Converted`: The serialized graph data.<br />`Format`: The format the graph was serialized in.<br />`Program`: The external program to launch.<br />

returns: The output of the program.

Launch an external program to view a serialized graph. A temporary
file is created to store the graph and passed into the external
program.

The external program will need to be installed and on the current
PATH for this to function.

This will block on the external program completing. Please spawn
this function in a separate process if that is not desired.

__See also:__ [formats/0](#formats-0), [programs/0](#programs-0).

