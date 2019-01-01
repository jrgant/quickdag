#' Output, view, or embed DAGs.
#'
#' Save directed acyclic graphs (DAGs) generated in DiagrammeR and export them to pdf, png, or svg format. Users may optionally choose to view the DAG without saving or to embed it in an RMarkdown document.
#'
#' @param graphcode Partial graphViz code object, which will give you the most control over the appearance of your DAG.
#' @param dagname A path with which to name your DAG file, if \code{filetype} set to output format (see below).
#' @param text.nodes A string containing the plain text nodes, separated by spaces.
#' @param box.nodes A string containing the boxed nodes, separated by spaces.
#' @param solid.edges A string specifying the paths you want to draw between measured covariates. Example: \code{"Alcohol -> Smoking Smoking -> Cancer"}.
#' @param dashed.edges A string specifying paths containing unmeasured covariates.
#' @param direction Specify the direction of diagram flow. Defaults to \code{"LR"}, for left-right.
#' @param filetype Output file format. Select from \code{pdf}, \code{png}, \code{svg}, or \code{view}. Setting the option to \code{view} will not save a file but will generate the diagram in your viewer. Defaults to \code{pdf}.
#' @param embed For use within R chunks in RMarkdown only. You will probably want to have the \code{echo} chunk option set to \code{FALSE}, unless you want to display the R code itself. The \code{embed} defaults to \code{TRUE}.
#' @param footnotes Add a footnote to the bottom of the graph.
#' @param ... Pass arguments to interior functions for PNG or SVG files. For instance, specify \code{height} and \code{width} arguments.
#'
#' @note
#' This is not a true DAG package in the sense that it will not prevent the inclusion of feedback loops or bidirectional arrows. It's meant mostly to create reasonable-looking DAGs quickly and easily with a minimum of layout or formatting code. DiagrammeR in general does a pretty good job at layout. Those interested in DAGs might check out other R packages like dagR or dagitty, both of which I've yet to explore in detail.
#'
#' Suggestions and issue reports welcome at \url{https://github.com/jrgant/quickDAG/issues}!
#'
#' @references
#' A fair amount of the heavy lifting here is done thanks to code snippets from users HJAllen and puterleat on the following thread: \url{https://github.com/rich-iannone/DiagrammeR/issues/133}
#'
#' Packages used: DiagrammeR, DiagrammeRsvg, rsvg
#'
#' @examples
#' # Using your own graph code
#'
#' dag <-
#'
#'      "graph[rankdir = LR]
#'
#'      node[shape = plaintext]
#'      Alcohol Smoking Cancer
#'
#'      node[shape = square]
#'      SES
#'
#'      edge[penwidth = .5, arrowsize = .5]
#'      SES -> Alcohol -> Cancer
#'      SES -> Smoking -> Cancer
#'      SES -> Cancer"
#'
#' makeDAG(dag, "mydag", filetype = "pdf")
#'
#' # Using the built-in defaults
#'
#' ## nodes
#' uncontrolled <- "Smoking Cancer Alcohol U"
#' controlled <- "SES"
#'
#' ## edges
#' measured <- "Smoking -> Alcohol -> Cancer
#'              SES -> Smoking -> Cancer
#'              SES -> Cancer"
#'
#' unmeasured <- "U -> Smoking
#'                U -> Cancer"
#'
#' makeDAG(dagname = "smk_ca", filetype = "pdf",
#' text.nodes = uncontrolled, box.nodes = controlled,
#' solid.edges = measured, dashed.edges = unmeasured)
#'
#' @export makeDAG
#' @import rsvg
#' @import DiagrammeR
#' @import DiagrammeRsvg
#' @import knitr


makeDAG <- function(graphcode = NULL, dagname = NULL, filetype = "pdf",
                    text.nodes = NULL, box.nodes = NULL,
                    solid.edges = NULL, dashed.edges = NULL, footnotes = NULL,
                    direction = "LR", embed = FALSE, ...) {

  message("makeDAG() will be deprecated in an upcoming version.")

  # make dag visible to function
  if (is.null(graphcode)) {

    dag_object <- DiagrammeR::grViz(
      paste("digraph{ graph[rankdir = ", direction, "]",
            "node[shape = plaintext, width = 0]", text.nodes,
            "node[shape = square, width = 0, penwidth = .5]", box.nodes,
            "edge[width = .1, arrowsize = .6, color = black]", solid.edges,
            "edge[width = .1, arrowsize = .6, color = black, style = dashed]", dashed.edges,
            "}")
    )

  } else {

    dag_object <- DiagrammeR::grViz(
      paste("digraph{ graph[rankdir = ", direction, "]", graphcode, "}", footnotes)
    )

  }

  # save DAG in selected format
  if (filetype == "pdf") {

    handle <- paste(dagname, ".pdf", sep = "")
    file.tmp <- utils::capture.output(rsvg_pdf(charToRaw(DiagrammeRsvg::export_svg(dag_object)),
                                               file = handle))

  } else if (filetype == "png") {

    handle <- paste(dagname, ".png", sep = "")
    file.tmp <- utils::capture.output(rsvg_png(charToRaw(DiagrammeRsvg::export_svg(dag_object)),
                                               file = handle, ...))

  } else if (filetype == "svg") {

    handle <- paste(dagname, ".svg", sep = "")
    file.tmp <- utils::capture.output(rsvg_svg(charToRaw(DiagrammeRsvg::export_svg(dag_object)),
                                               file = handle, ...))

  } else if (filetype == "eps") {

    handle <- paste(dagname, ".eps", sep = "")
    file.tmp <- utils::capture.output(rsvg_ps(charToRaw(DiagrammeRsvg::export_svg(dag_object)),
                                               file = handle, ...))

  } else if (filetype == "view") {

    print(dag_object)

  } else {

    stop("Filetype not supported. Select one of: pdf, png, svg, eps, view.")

  }

  # check for embed directive: RMarkdown only
  if (embed) {

    knitr::include_graphics(handle)

  }

}
