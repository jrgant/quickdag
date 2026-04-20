# Diagram themes

Themes and theming utilies.

## Usage

``` r
qd_themes(graph_obj, theme, conditioned = NULL)

theme_qd_base(
  graph_obj,
  rankdir = getOption("quickdag.base_rankdir"),
  layout = getOption("quickdag.base_layout"),
  shape = getOption("quickdag.base_shape"),
  nodepen = getOption("quickdag.base_nodepen"),
  nodewidth = getOption("quickdag.base_nodewidth"),
  nodeheight = getOption("quickdag.base_nodeheight"),
  edgepen = getOption("quickdag.base_edgepen"),
  arrowhead = getOption("quickdag.base_arrowhead"),
  arrowsize = getOption("quickdag.base_arrowsize"),
  fontname = getOption("quickdag.base_fontname"),
  fontsize = getOption("quickdag.base_fontsize"),
  fontcolor = getOption("quickdag.base_fontcolor"),
  conditioned = NULL
)

theme_qd_circles(
  graph_obj,
  nodepen = choose_option("quickdag.circles_nodepen"),
  edgepen = choose_option("quickdag.circles_edgepen"),
  fontname = choose_option("quickdag.circles_fontname"),
  fontsize = choose_option("quickdag.circles_fontsize"),
  fontcolor = choose_option("quickdag.circles_fontcolor"),
  conditioned = NULL
)

theme_qd_pearl(
  graph_obj,
  pointsize = getOption("quickdag.pearl_pointsize"),
  pointcolor = getOption("quickdag.pearl_pointcolor"),
  pointfill = getOption("quickdag.pearl_pointfill"),
  edgepen = choose_option("quickdag.pearl_edgepen"),
  arrowhead = choose_option("quickdag.pearl_arrowhead"),
  arrowsize = choose_option("quickdag.pearl_arrowsize"),
  fontname = choose_option("quickdag.pearl_fontname"),
  fontsize = choose_option("quickdag.pearl_fontsize"),
  fontcolor = choose_option("quickdag.pearl_fontcolor"),
  conditioned = NULL
)

get_conditioned_nodes(graph_obj, conditioned = NULL)

cleanup_existing_theme(graph_obj)
```

## Arguments

- graph_obj:

  A DAG object created by
  [`qd_dag()`](https://jrgant.github.io/quickdag/reference/qd_dag.md).

- theme:

  A character string indicating the theme to use. Defaults to "base".
  Set to `NULL` to use GraphViz defaults.

- conditioned:

  A character vector indicating which nodes are conditioned upon. The
  shape for these nodes will be set to "rectangle".

- rankdir:

  Direction of node layout. Defaults to "LR", for left-to-right.

- layout:

  Layout engine. Defaults to "dot", which is the only engine explicitly
  used by
  [quickdag](https://jrgant.github.io/quickdag/reference/quickdag-package.md).
  Other options provided by
  [DiagrammeR::DiagrammeR](https://rich-iannone.github.io/DiagrammeR/reference/DiagrammeR.html)
  may or may not work well.

- shape:

  Standard node shape for a given graph. Defaults for each theme: "base"
  = plaintex, "circles" = circle, "pearl" = point. See [Graphviz
  documentation](https://graphviz.org/doc/info/shapes.html) for other
  options.

- nodepen:

  Size (in points) of node outlines. By default, controlled by package
  options specific to each theme, each of which defaults to 0.2.

- nodewidth:

  Width (in inches) of nodes. By default, controlled by package options
  specific to each theme. Defaults to 0.

- nodeheight:

  Height (in inches) of nodes. By default, controlled by package options
  specific to each theme. Defaults to 0.

- edgepen:

  Size (in points) of edges. By default, controlled by package options
  specific to each theme, each of which defaults to 0.2.

- arrowhead:

  Shape of the arrowhead. See
  [`DiagrammeR::edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.html).

- arrowsize:

  Size of the arrowhead. See
  [`DiagrammeR::edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.html).

- fontname:

  Font name for text elements of the graph. By default, controlled by
  package options specific to each theme, each of which defaults to
  "Helvetica". This is the default in `DiagrammeR`.

- fontsize:

  Size of text (in points). By default, controlled by package options
  specific to each theme, each of which defaults to 5.

- fontcolor:

  Text color. By default, controlled by package options specific to each
  theme, each of which defaults to "black".

- pointsize:

  Size of "point" shape (in points). Applies to "pearl" theme only and
  defaults to 0.02.

- pointcolor:

  Border color for the "point" shape. Applies to "pearl" theme only and
  defaults to "black".

- pointfill:

  Fill color for the "point" shape. Applies to "pearl" theme only and
  defaults to "black".
