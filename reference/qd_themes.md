# Diagram themes

Apply various pre-fabricated themes to diagrams.

## Usage

``` r
qd_themes(graph_obj, theme, ...)

theme_qd_base(graph_obj, font = "serif", ...)

theme_qd_circles(graph_obj, font = "serif", ...)

theme_qd_pearl(graph_obj, font = "serif", ...)

get_conditioned_nodes(graph_obj, conditioned = NULL)
```

## Arguments

- graph_obj:

  A DAG object created by
  [`qd_dag()`](https://jrgant.github.io/quickdag/reference/qd_dag.md).

- theme:

  A character string indicating the theme to use. Defaults to "base".
  Set to `NULL` to use GraphViz defaults.

- ...:

  Pass arguments to theme call (e.g., `theme_qd_base()`), such as
  `conditioned` or `font`.

- font:

  A character vector indicating the font family to use for node labels.
  Defaults to "serif".

- conditioned:

  A character vector indicating which nodes are conditioned upon. The
  shape for these nodes will be set to "square".
