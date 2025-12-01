# Put some variables in the global environment to avoid R CMD CHECK notes
utils::globalVariables(c("alpha_id", "label", "start", "end"))

.onLoad <- function(libname, pkgname) {
  opts <- list(
    quickdag.check_dag = TRUE,
    quickdag.verbose = FALSE,
    quickdag.format_special = FALSE,
    quickdag.fixed_sep = "vlin",
    quickdag.theme = "base",
    quickdag.embed = FALSE
  )
  options(opts)
}
