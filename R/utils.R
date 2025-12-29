# Put some variables in the global environment to avoid R CMD CHECK notes
utils::globalVariables(c("alpha_id", "label", "start", "end"))

# Set global options
.onLoad <- function(libname, pkgname) {
  opts <- list(
    quickdag.check_dag = TRUE,
    quickdag.embed = FALSE,
    quickdag.fixed_sep = "vlin",
    quickdag.format_special = FALSE,
    quickdag.theme = "base",
    quickdag.verbose = FALSE,
    quickdag.pearl_pointsize = 0.02,
    quickdag.pearl_linewidths = 0.2,
    quickdag.pearl_fontsize = 5,
    quickdag.pearl_fontcolor = "black"
  )
  options(opts)
}
