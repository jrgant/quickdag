# Generate a single-world intervention graph (SWIG)

Take a DAG graph object and, in the simplest case, create a single-world
intervention template corresponding to a world in which the fixed nodes
are set to a given value. Alternatively, tell `qd_swig()` which values
fixed nodes will be set to.

## Usage

``` r
qd_swig(
  graph_obj,
  fixed_nodes,
  custom_values = NULL,
  fixed_sep = getOption("quickdag.swig_fixedsep"),
  sep_point_size = getOption("quickdag.swig_sepsize")
)
```

## Arguments

- graph_obj:

  A DAG object created by
  [`qd_dag()`](https://jrgant.github.io/quickdag/reference/qd_dag.md).

- fixed_nodes:

  A vector containing the nodes to be intervened upon.

- custom_values:

  A named vector containing alternative labels identifying explicit
  values for fixed nodes (e.g., `c("A" = "1")`).

- fixed_sep:

  A character string indicating which character to use as a separator in
  fixed nodes. Defaults to `"vlin"`. Run `sep_opts(TRUE)` for available
  options.

- sep_point_size:

  A numerical value specifying the point size for fixed node separators.

## Examples

``` r
# Provide a DAG object and a list of nodes to be fixed
edges <- c("A -> Y",
           "L -> { A Y }")

dag  <- qd_dag(edges)
#> 
#> 
#> CHECKED: The diagram is a DAG. 

swig <- dag |>
        qd_swig(fixed_nodes = "A",
                custom_values = c("A" = "1"))

swig |> DiagrammeR::render_graph()

{"x":{"diagram":"digraph {\n\ngraph [rankdir = \"LR\",\n       layout = \"dot\"]\n\n\n\n  \"1\" [label = <A <font point-size=\"15\">&#124;<\/font> <i>a=1<\/i><FONT POINT-SIZE=\"8\"><SUB> <\/SUB><\/FONT>>, shape = \"plaintext\", penwidth = \"0.5\", fontname = \"Helvetica\", width = \"0\", height = \"0\", fillcolor = \"#FFFFFF\", fontcolor = \"#000000\"] \n  \"2\" [label = <Y<FONT POINT-SIZE=\"8\"><SUP><i>a=1<\/i><\/SUP><\/FONT>>, shape = \"plaintext\", penwidth = \"0.5\", fontname = \"Helvetica\", width = \"0\", height = \"0\", fillcolor = \"#FFFFFF\", fontcolor = \"#000000\"] \n  \"3\" [label = \"L\", shape = \"plaintext\", penwidth = \"0.5\", fontname = \"Helvetica\", width = \"0\", height = \"0\", fillcolor = \"#FFFFFF\", fontcolor = \"#000000\"] \n\"1\"->\"2\" [arrowhead = \"normal\", arrowsize = \"0.4\", penwidth = \"0.5\"] \n\"3\"->\"1\" [arrowhead = \"normal\", arrowsize = \"0.4\", penwidth = \"0.5\"] \n\"3\"->\"2\" [arrowhead = \"normal\", arrowsize = \"0.4\", penwidth = \"0.5\"] \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
```
