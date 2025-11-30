#' @title Identify variables for adjustment
#'
#' @description
#' Format an edgelist and send it to dagitty to identify variable adjustment sets.
#'
#' @param edgelist A vector of edge relationships.
#' @param diagram_type Character identifying the diagram type. Defaults to "dag", but
#'   user can specify another graph type (see [dagitty::adjustmentSets()]).
#' @param showplot Logical indicating whether to produce a dagitty plot. Defaults to
#'   `FALSE`.
#' @param exposure Character. Specify exposure of interest. (Required)
#' @param outcome Character. Specifiy outcome of interest. (Required)
#' @param ... Pass arguments to [dagitty::adjustmentSets()].
#'
#' @details
#' The `exposure` and `outcome` options map to dagitty parameters of the same name.
#'
#' [qd_todagitty()] remains an alias for [qd_adjustment_sets()] to avoid breaking
#' existing scripts, as it was the original name of this function.
#'
#' @rdname qd_adjustment_sets
#' @export
#' @examples
#' # feed an edgelist to qd_adjustment_sets()
#' edges <- c("A -> { B C D }",
#'            "B -> C",
#'            "E -> { B C }",
#'            "Y <- L -> A")
#' qd_adjustment_sets(edges, exposure = "A", outcome = "C")
#' qd_adjustment_sets(edges, exposure = "A", outcome = "C", type = "minimal")
#'
#' # if you've already created a qd_dag() object
#' dag <- qd_dag(edges)
#' qd_adjustment_sets(dag$qd_edgelist)
qd_adjustment_sets <- function(edgelist, diagram_type = "dag", showplot = FALSE,
                               exposure, outcome,
                               ...) {

  dagitty_obj <- dagitty::dagitty(
    paste(diagram_type, "{", paste(edgelist, collapse = "; "), "}"),
    layout = TRUE
  )

  ## optional to show dagitty plot
  if (showplot) {
    plot(dagitty_obj)
  }

  ## use dagitty's algorithm to identify adjustment sets
  sets <- dagitty::adjustmentSets(dagitty_obj,
                                  exposure = exposure,
                                  outcome = outcome,
                                  ...)
  return(sets)
}


#' @rdname qd_adjustment_sets
#' @export
qd_todagitty <- qd_adjustment_sets
