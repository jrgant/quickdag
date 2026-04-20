# Identify variables for adjustment

Format an edgelist and send it to dagitty to identify variable
adjustment sets.

## Usage

``` r
qd_adjustment_sets(
  edgelist,
  diagram_type = "dag",
  showplot = FALSE,
  exposure,
  outcome,
  ...
)

qd_todagitty(
  edgelist,
  diagram_type = "dag",
  showplot = FALSE,
  exposure,
  outcome,
  ...
)
```

## Arguments

- edgelist:

  A vector of edge relationships.

- diagram_type:

  Character identifying the diagram type. Defaults to "dag", but user
  can specify another graph type (see
  [`dagitty::adjustmentSets()`](https://rdrr.io/pkg/dagitty/man/adjustmentSets.html)).

- showplot:

  Logical indicating whether to produce a dagitty plot. Defaults to
  `FALSE`.

- exposure:

  Character. Specify exposure of interest. (Required)

- outcome:

  Character. Specifiy outcome of interest. (Required)

- ...:

  Pass arguments to
  [`dagitty::adjustmentSets()`](https://rdrr.io/pkg/dagitty/man/adjustmentSets.html).

## Details

The `exposure` and `outcome` options map to dagitty parameters of the
same name.

`qd_todagitty()` remains an alias for `qd_adjustment_sets()` to avoid
breaking existing scripts, as it was the original name of this function.

## Examples

``` r
# feed an edgelist to qd_adjustment_sets()
edges <- c("A -> { B C D }",
           "B -> C",
           "E -> { B C }",
           "Y <- L -> A")
qd_adjustment_sets(edges, exposure = "A", outcome = "C")
#>  {}
qd_adjustment_sets(edges, exposure = "A", outcome = "C", type = "minimal")
#>  {}

# if you've already created a qd_dag() object
dag <- qd_dag(edges)
#> 
#> 
#> CHECKED: The diagram is a DAG. 
qd_adjustment_sets(dag$qd_edgelist, exposure = "A", outcome = "Y")
#> { L }
```
