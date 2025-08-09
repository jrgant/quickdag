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
#' @param send_global Logical indicating whether to make the dagitty object available
#'   in the global environment. Defaults to `FALSE`.
#' @param dagitty_obj_name Character specifying the name of the dagitty object. Only
#'   used and required if `send_global = TRUE`.
#' @param exposure Character. Specify exposure of interest. (Required)
#' @param outcome Character. Specifiy outcome of interest. (Required)
#' @param ... Pass arguments to [dagitty::adjustmentSets()]. See dagitty documentation
#'   for options.
#'
#' @details
#'
#' The `exposure` and `outcome` options map to dagitty functions of the same name.
#'
#' @export qd_todagitty
#' @examples
#' edges <- c("A -> { B C D }",
#'            "B -> C",
#'            "E -> { B C }")
#' # must pass exposure and outcome arguments to dagitty::adjustmentSets()
#' qd_todagitty(edges, exposure = "A", outcome = "C")
#' qd_todagitty(edges, exposure = "A", outcome = "C", type = "minimal")
qd_todagitty <- function(edgelist, diagram_type = "dag", showplot = FALSE,
                         send_global = FALSE, dagitty_obj_name = NULL,
                         exposure, outcome,
                         ...) {

  dagitty_obj <- dagitty::dagitty(paste(diagram_type, "{",
                                        paste(edgelist, collapse = "; "),
                                        "}"),
                                  layout = TRUE)

  # make object available in global environment (defaults to TRUE)
  if (send_global) {
    if (is.null(dagitty_obj_name)) {
      stop("Provide a name for your DAG.")
    }
    assign(dagitty_obj_name, dagitty_obj, envir = .GlobalEnv)
  }

  ## optional to show dagitty plot
  if (showplot) {
    plot(dagitty_obj)
  }

  ## use dagitty's algorithm to identify adjustment sets
  sets <- dagitty::adjustmentSets(dagitty_obj, exposure = exposure,
                                  outcome = outcome, ...)
  return(sets)
}
