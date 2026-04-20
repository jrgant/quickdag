# Generate a graph object

Provide simple syntax specifying paths between nodes to generate a graph
object.

## Usage

``` r
qd_dag(
  edgelist,
  node_labs = NULL,
  node_aes_opts = list(),
  edge_aes_opts = list(),
  format_special = getOption("quickdag.format_special"),
  verbose = getOption("quickdag.verbose"),
  check_dag = getOption("quickdag.check_dag"),
  theme = getOption("quickdag.theme"),
  ...
)
```

## Arguments

- edgelist:

  A vector of node-edge relationships.

- node_labs:

  A named character vector containing label names. Defaults to `NULL`.

- node_aes_opts:

  A list feeding aesthetic options for nodes to
  [`DiagrammeR::node_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/node_aes.html).
  Defaults to empty list.

- edge_aes_opts:

  A list feeding aesthetic options for edges to
  [`DiagrammeR::edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.html).
  Defaults to empty list.

- format_special:

  Render numeric elements in an alphanumeric `alpha_id` as subcripts.
  Defaults to `FALSE`.

- verbose:

  Indicate whether to print node and edge dataframes to the console. See
  Details below. Defaults to `TRUE`.

- check_dag:

  Logical. Check whether the graph conforms to the rules of DAGs.
  Defaults to `TRUE`.

- theme:

  Choose theme for plot output. Defaults to `"base"`. Setting theme to
  `NULL` will use DiagrammeR's `NULL` attribute theme.

- ...:

  Pass optional `conditioned` argument to
  [`qd_themes()`](https://jrgant.github.io/quickdag/reference/qd_themes.md).

## Details

Leaving the `verbose` set to `TRUE` may be advisable to ensure labels
and IDs have not been mismatched. By default, `qd_dag()` alphabetizes
nodes included in `edgelist` and does the same for `node_labs` under a
first assumption that labels will begin with the same letter as their
corresponding `alpha_id`, which may not always be the case.

## Examples

``` r
# Provide a list of edges, with nodes specified as letters or single words.
edges <- c("A -> { B C } <- L",
           "B -> C")

# Make a DAG object and render the graph using the default theme
g.obj <- qd_dag(edges)
#> 
#> 
#> CHECKED: The diagram is a DAG. 
DiagrammeR::render_graph(g.obj)

{"x":{"diagram":"digraph {\n\ngraph [rankdir = \"LR\",\n       layout = \"dot\"]\n\n\n\n  \"1\" [label = \"A\", shape = \"plaintext\", penwidth = \"0.5\", fontname = \"Helvetica\", width = \"0\", height = \"0\", fillcolor = \"#FFFFFF\", fontcolor = \"#000000\"] \n  \"2\" [label = \"B\", shape = \"plaintext\", penwidth = \"0.5\", fontname = \"Helvetica\", width = \"0\", height = \"0\", fillcolor = \"#FFFFFF\", fontcolor = \"#000000\"] \n  \"3\" [label = \"C\", shape = \"plaintext\", penwidth = \"0.5\", fontname = \"Helvetica\", width = \"0\", height = \"0\", fillcolor = \"#FFFFFF\", fontcolor = \"#000000\"] \n  \"4\" [label = \"L\", shape = \"plaintext\", penwidth = \"0.5\", fontname = \"Helvetica\", width = \"0\", height = \"0\", fillcolor = \"#FFFFFF\", fontcolor = \"#000000\"] \n\"1\"->\"2\" [arrowhead = \"normal\", arrowsize = \"0.4\", penwidth = \"0.5\"] \n\"1\"->\"3\" [arrowhead = \"normal\", arrowsize = \"0.4\", penwidth = \"0.5\"] \n\"4\"->\"2\" [arrowhead = \"normal\", arrowsize = \"0.4\", penwidth = \"0.5\"] \n\"4\"->\"3\" [arrowhead = \"normal\", arrowsize = \"0.4\", penwidth = \"0.5\"] \n\"2\"->\"3\" [arrowhead = \"normal\", arrowsize = \"0.4\", penwidth = \"0.5\"] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
# Pass labels and aesthetic options for nodes or edges
g.obj2 <- qd_dag(edges,
                 node_labs = c("A" = "Alcohol",
                               "B" = "BP",
                               "C" = "CVD",
                               "L" = "State"),
                 node_aes_opts = list(shape = "plaintext",
                                      fillcolor = "none",
                                      color = "black"),
                 edge_aes_opts = list(arrowsize = 0.5,
                                      color = "gray"),
                 theme = NULL)
#> 
#> 
#> CHECKED: The diagram is a DAG. 
DiagrammeR::render_graph(g.obj2)

{"x":{"diagram":"digraph {\n\n\n\n\n  \"1\" [label = \"Alcohol\", shape = \"plaintext\", fillcolor = \"#FFFFFE\", color = \"black\", fontcolor = \"#000000\"] \n  \"2\" [label = \"BP\", shape = \"plaintext\", fillcolor = \"#FFFFFE\", color = \"black\", fontcolor = \"#000000\"] \n  \"3\" [label = \"CVD\", shape = \"plaintext\", fillcolor = \"#FFFFFE\", color = \"black\", fontcolor = \"#000000\"] \n  \"4\" [label = \"State\", shape = \"plaintext\", fillcolor = \"#FFFFFE\", color = \"black\", fontcolor = \"#000000\"] \n\"1\"->\"2\" [arrowsize = \"0.5\", color = \"gray\"] \n\"1\"->\"3\" [arrowsize = \"0.5\", color = \"gray\"] \n\"4\"->\"2\" [arrowsize = \"0.5\", color = \"gray\"] \n\"4\"->\"3\" [arrowsize = \"0.5\", color = \"gray\"] \n\"2\"->\"3\" [arrowsize = \"0.5\", color = \"gray\"] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
```
