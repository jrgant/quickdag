#' @title Identify variables for adjustment
#'
#' @description
#' Format an edgelist and send it to dagitty to identify variable adjustment sets.
#'
#' @param edgelist A vector of edge relationships. Must be strictly organized (see
#'   example for format).
#' @param diagram_type Character identifying the diagram type. Defaults to "dag", but
#'   user can specify another graph type (see dagitty documentation).
#' @param showplot Logical indicating whether to produce a dagitty plot. Defaults to
#'   `FALSE`.
#' @param exposure Character. Specify exposure of interest. (Required)
#' @param outcome Character. Specifiy outcome of interest. (Required)
#' @param ... Pass arguments to [dagitty::adjustmentSets()]. See dagitty documentation
#'   for options.
#'
#' @details
#' The `exposure` and `outcome` options map to dagitty functions of the same name.
#'
#' @export
#' @examples
#' edges <- c("A -> { B C D }",
#'            "B -> C",
#'            "E -> { B C }")
#' # must pass exposure and outcome arguments to dagitty::adjustmentSets()
#' qd_todagitty(edges, exposure = "A", outcome = "C")
#' qd_todagitty(edges, exposure = "A", outcome = "C", type = "minimal")
qd_todagitty <- function(edgelist, diagram_type = "dag", showplot = FALSE,
                         exposure, outcome,
                         ...) {

  dagitty_obj <- dagitty::dagitty(paste(diagram_type, "{",
                                        paste(edgelist, collapse = "; "),
                                        "}"),
                                  layout = TRUE)

  ## optional to show dagitty plot
  if (showplot) {
    plot(dagitty_obj)
  }

  ## use dagitty's algorithm to identify adjustment sets
  sets <- dagitty::adjustmentSets(dagitty_obj, exposure = exposure,
                                  outcome = outcome, ...)
  return(sets)
}
