#' Package Setup
#'
#' A temporary workaround until devtools::install_github gets a bugfix that properly installs all package dependencies (and sub-dependencies).
#'
#' @export quickDAG_setup

quickDAG_setup <- function() {

  require(pacman)
  pacman::p_load(DiagrammeR, DiagrammeRsvg)

}
