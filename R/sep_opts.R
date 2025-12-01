#' View options for fixed node separator characters
#'
#' @description
#' Preview character options for use as the fixed node separator in SWIGs.
#'
#' @param entity The separator name or an HTML entity of the user's choosing. Defaults
#'   to `NULL`.
#' @param table Logical to show or hide HTML table display of built-in characters.
#'   Defaults to `FALSE`.
#'
#' @details
#' By default [sep_opts()] returns a named vector listing the built-in separators. These
#' built-in HTML entities are attached to nicenames for convenience. If the user provides
#' the nicename for a built-in entity to the `entity` argument, the function will return
#' the appropriate HTML code. If the user provides an HTML entity that does not appear in
#' the built-in menu, the input will be returned verbatim. Users will typically access
#' this latter functionality via [qd_swig()].
#'
#' @export
sep_opts <- function(entity = NULL, table = FALSE) {
  defopts <- c("tilde"  = "&#8768;",
               "vsep"   = "&#8739;",
               "vlin"   = "&#124;",
               "vdubs"  = "&#8214;",
               "vdubl"  = "&#8741;",
               "sol"    = "&#47;",
               "soldub" = "&#11005;",
               "bracks" = "][",
               "rangle" = "&#10217;")
  if (table == TRUE) {
    char_tab <- tibble::tibble(separator = names(defopts),
                               result = unname(defopts))
    print(
      htmlTable::htmlTable(
        char_tab,
        rnames = FALSE,
        css.table = "font-size: 1.5em; font-family: Arial, sans-serif;",
        css.cell = "padding: 0.75em 1.5em;"
      )
    )
  }

  if (is.null(entity)) {
    defopts
  } else if (entity %in% names(defopts)) {
    defopts[names(defopts) == entity]
  } else {
    entity
  }
}
