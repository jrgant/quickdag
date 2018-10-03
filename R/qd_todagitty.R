#' @title Identify variables for adjustment
#'
#' @description Format an edgelist and send it to dagitty to identify variable adjustment sets.
#'
#' @param edgelist A vector of edge relationships. Must be strictly organized (see example for format).
#' @param diagram_type Character identifying the diagram type. Defaults to \code{"dag"}, but user can specify another graph type (see dagitty documentation).
#' @param showplot Logical indicating whether to produce a dagitty plot. Defaults to \code{FALSE}.
#' @param send.global Logical indicating whether to make the dagitty object available in the global environment. Defaults to \code{FALSE}.
#' @param dagitty.obj.name Character specifying the name of the dagitty object. Only used and required if \code{send.global = TRUE}.
#' @param exposure Character. Specify exposure of interest. (Required)
#' @param outcome Character. Specifiy outcome of interest. (Required)
#' @param ... Pass arguments to \code{adjustmentSets()}. See dagitty documentation for options.
#'
#' @note
#'
#' The \code{exposure} and \code{outcome} options map to dagitty functions of the same name.
#'
#' @examples
#' edges <- c("A -> { B C D }",
#'            "B -> C",
#'            "E -> { B C }")
#'
#' # must pass exposure and outcome arguments to dagitty::adjustmentSets()
#' qd_todagitty(edges, exposure = "A", outcome = "C")
#' #' qd_todagitty(edges, exposure = "A", outcome = "C", type = "minimal")
#'
#' @importFrom dagitty dagitty
#' @importFrom dagitty adjustmentSets
#' @export qd_todagitty


qd_todagitty <- function(edgelist, diagram_type = "dag", showplot = FALSE,
                         send.global = FALSE, dagitty.obj.name = NULL,
                         ...) {

  dagitty.obj <- dagitty::dagitty(paste(diagram_type, "{",
                                        paste(edgelist, collapse = "; "),
                                        "}"),
                                  layout = TRUE)

  # make object available in global environment (defaults to TRUE)
  if (send.global) {
    if (is.null(dagitty.obj.name)) {
      stop("Provide a name for your DAG.")
    }
    assign(dagitty.obj.name, dagitty.obj, envir = .GlobalEnv)
  }

  ## optional to show dagitty plot
  if (showplot) {
    plot(dagitty.obj)
  }

  ## use dagitty's algorithm to identify adjustment sets
  sets <- dagitty::adjustmentSets(dagitty.obj, ...)
  return(sets)
}
