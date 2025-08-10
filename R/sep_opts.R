#' View options for fixed node separator characters
#'
#' @description
#' Preview character options for use as the fixed node separator in SWIGs.
#'
#' @param table Logical to show or hide HTML table display of available characters.
#'   Defaults to `FALSE`.
#'
#' @export
sep_opts <- function(table = FALSE) {
  sep_opts <- c("tilde" = "&#8768;",
                "vsep" = "&#8739;",
                "vlin" = "&#124;",
                "vdubs" = "&#8214;",
                "vdubl" = "&#8741;",
                "sol" = "&#47;",
                "soldub" = "&#11005;",
                "bracks" = "][",
                "rangle" = "&#10217;")
  if (table == TRUE) {
    char_tab <- tibble::tibble(separator = names(sep_opts),
                               result = unname(sep_opts))
    print(
      htmlTable::htmlTable(
        char_tab,
        rnames = FALSE,
        css.table = "font-size: 1.5em; font-family: Arial, sans-serif;",
        css.cell = "padding: 0.75em 1.5em;"
      )
    )
  }
  return(sep_opts)
}
