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
  fixed_sep = "vlin",
  sep_point_size = 15
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

{"x":{"diagram":"digraph {\n\ngraph [rankdir = \"LR\",\n       layout = \"dot\"]\n\nnode [shape = \"plaintext\",\n      penwidth = \"0.5\",\n      fontname = \"serif\",\n      width = \"0\",\n      height = \"0\"]\n\nedge [arrowsize = \"0.4\",\n     penwidth = \"0.5\"]\n\n  \"1\" [label = <A <font point-size=\"15\">&#124;<\/font> <i>a=1<\/i><FONT POINT-SIZE=\"8\"><SUB> <\/SUB><\/FONT>>, fillcolor = \"#FFFFFF\", fontcolor = \"#000000\"] \n  \"2\" [label = <Y<FONT POINT-SIZE=\"8\"><SUP><i>a=1<\/i><\/SUP><\/FONT>>, fillcolor = \"#FFFFFF\", fontcolor = \"#000000\"] \n  \"3\" [label = \"L\", fillcolor = \"#FFFFFF\", fontcolor = \"#000000\"] \n  \"1\"->\"2\" \n  \"3\"->\"1\" \n  \"3\"->\"2\" \n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}
```
