# Graph operations

Graph operations

## Usage

``` r
qd_set_node_attrs(graph_obj, ..., alpha_ids)

qd_set_edge_attrs(graph_obj, ..., from_alpha = NULL, to_alpha = NULL)

select_nodes_by_alpha_id(graph_obj, alpha_ids, set_op = "union")

qd_select_nodes(graph_obj, alpha_ids, set_op = "union")

select_edges_by_node_alpha_id(graph_obj, alpha_ids, set_op = "union")

qd_select_edges(graph_obj, alpha_ids, set_op = "union")
```

## Arguments

- graph_obj:

  A `quickdag` object output by
  [`qd_dag()`](https://jrgant.github.io/quickdag/reference/qd_dag.md) or
  [`qd_swig()`](https://jrgant.github.io/quickdag/reference/qd_swig.md).

- ...:

  Passed to
  [`DiagrammeR::set_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs.html)
  or
  [`DiagrammeR::set_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs.html)
  argument `node_attr` or `edge_attr`, respectively.
  [`DiagrammeR::set_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs.html)
  argument of the same name.

- alpha_ids:

  A vector of alphanumeric node IDs upon which to operate.

- from_alpha:

  A vector of alphanumeric source node IDs.

- to_alpha:

  A vector of alphanumeric destination node IDs.

- set_op:

  Passed to the `set_op` argument of
  [`DiagrammeR::select_nodes_by_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_id.html)
  or
  [`DiagrammeR::select_edges_by_node_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges_by_node_id.html).
  Defaults to "union".
