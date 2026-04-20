# Retrieves numeric IDs given alphanumeric IDs

Retrieves numeric IDs given alphanumeric IDs

## Usage

``` r
get_numids(graph_obj, alpha_ids)
```

## Arguments

- graph_obj:

  A `quickdag` object output by
  [`qd_dag()`](https://jrgant.github.io/quickdag/reference/qd_dag.md) or
  [`qd_swig()`](https://jrgant.github.io/quickdag/reference/qd_swig.md).

- alpha_ids:

  A vector of alphanumeric node IDs upon which to operate.
