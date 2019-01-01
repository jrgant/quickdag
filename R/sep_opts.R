#' View options for fixed node separator characters
#'
#' @description
#' Provide simple syntax specifying paths between nodes to generate a graph object.
#'
#' @param table Logical to show or hide HTML table display of available characters. Defaults to \code{FALSE}.
#'
#' @export sep_opts
#' @importFrom dplyr data_frame
#' @importFrom htmlTable htmlTable

sep_opts <- function(table = FALSE) {
  sep.opts <- c("tilde" = "&#8768;",
                "vsep" = "&#8739;",
                "vlin" = "&#124;",
                "vdubs" = "&#8214;",
                "vdubl" = "&#8741;",
                "sol" = "&#47;",
                "soldub" = "&#11005;",
                "bracks" = "][",
                "rangle" = "&#10217;")
  if (table) {
    char.tab <- data_frame(separator = names(sep.opts),
                           result = unname(sep.opts))
    print(
      htmlTable(char.tab,
                rnames = FALSE,
                css.table = "font-size: 1.5em; font-family: Arial, sans-serif;",
                css.cell = "padding: 0.75em 1.5em;")
      )
  }
  return(sep.opts)
}

