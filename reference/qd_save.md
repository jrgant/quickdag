# Save graph objects as images

`qd_save()` is a light wrapper around
[`DiagrammeR::export_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/export_graph.html),
which enables exporting high-quality and/or scalable graphics for both
print and online. For convenience in computational notebooks like quarto
or RMarkdown documents, `qd_save()` also allows the user to embed the
image via
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html).

## Usage

``` r
qd_save(graph, file_name, ..., embed = FALSE, kg = NULL)
```

## Arguments

- graph:

  A graph object produced by
  [`qd_dag()`](https://jrgant.github.io/quickdag/reference/qd_dag.md)

- file_name:

  A filename (possibly including a path) to which to save the image.
  File type will be determined by the extension provided (pdf, svg, png,
  ps). See
  [`DiagrammeR::export_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/export_graph.html)
  for details.

- ...:

  Pass other arguments to
  [`DiagrammeR::render_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph.html).

- embed:

  Defaults to `FALSE`.

- kg:

  A list allowing the user to set arguments for
  [`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html)
  (except for `path`).

## Examples

``` r
# redirect file to temporary directory (example only)
file <- file.path(tempdir(), "dag.pdf")

# save without embedding
dag <- qd_dag(c("L -> {A Y}", "A -> Y"),
              check_dag = FALSE, verbose = FALSE)
qd_save(dag, file)

# embed from code chunk using knitr::include_graphics()
qd_save(dag, file, embed = TRUE)
#> [1] "/tmp/RtmpnnzWF3/dag.pdf"
#> attr(,"class")
#> [1] "knit_image_paths" "knit_asis"       

# `title` passed to DiagrammeR::export_graph()
qd_save(dag, file, title = "Demo")

# clean up temporary directory
file.remove(file)
#> [1] TRUE
```
