#' @title Save graph objects as images
#'
#' @description
#' [qd_save()] is a light wrapper around [DiagrammeR::export_graph()], which
#' enables exporting high-quality and/or scalable graphics for both print and online.
#' For convenience in computational notebooks like quarto or RMarkdown documents,
#' [qd_save()] also allows the user to embed the image via [knitr::include_graphics()].
#'
#' @param graph A graph object produced by [qd_dag()]
#' @param file_name A filename (possibly including a path) to which to save the image.
#'   File type will be determined by the extension provided (pdf, svg, png, ps). See
#'   [DiagrammeR::export_graph()] for details.
#' @param ... Pass other arguments to [DiagrammeR::render_graph()].
#' @param embed Defaults to `FALSE`. Automatically set to `TRUE` by [qd_embed()].
#' @param kg A list allowing the user to set arguments for [knitr::graphics()] (except for
#'   `path`).
#' @rdname qd_save
#' @export
#' @examples
#' # redirect file to temporary directory (example only)
#' file <- file.path(tempdir(), "dag.pdf")
#'
#' # save without embedding
#' dag <- qd_dag(c("L -> {A Y}", "A -> Y"))
#' qd_save(dag, file)
#'
#' # embed from code chunk using knitr::include_graphics()
#' qd_save(dag, file, embed = TRUE)
#'
#' # `title` passed to DiagrammeR::export_graph()
#' qd_save(dag, file, title = "Demo")
#'
#' # clean up temporary directory
#' file.remove(file)
qd_save <- function(graph, file_name, ..., embed = FALSE, kg = NULL) {
  DiagrammeR::export_graph(graph = graph, file_name = file_name, ...)

  if (embed == TRUE) {
    kg_args <- list(path = file_name)
    if (!is.null(kg)) {
      kg_args <- c(kg_args, kg)
    }
    do.call(knitr::include_graphics, args = kg_args)
  }
}
