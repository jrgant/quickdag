# Identify variables for adjustment

Format an edgelist and send it to dagitty to identify variable
adjustment sets.

## Usage

``` r
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

  A vector of edge relationships. Must be strictly organized (see
  example for format).

- diagram_type:

  Character identifying the diagram type. Defaults to "dag", but user
  can specify another graph type (see dagitty documentation).

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
  See dagitty documentation for options.

## Details

The `exposure` and `outcome` options map to dagitty functions of the
same name.

## Examples

``` r
edges <- c("A -> { B C D }",
           "B -> C",
           "E -> { B C }")
# must pass exposure and outcome arguments to dagitty::adjustmentSets()
qd_todagitty(edges, exposure = "A", outcome = "C")
#>  {}
qd_todagitty(edges, exposure = "A", outcome = "C", type = "minimal")
#>  {}
```
