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
    quickdag.base_linewidths = 0.2,
    quickdag.base_fontname = "Helvetica", # DiagrammeR's default
    quickdag.base_fontsize = 5,
    quickdag.base_fontcolor = "black",
    quickdag.circles_linewidths = 0.2,
    quickdag.circles_fontname = "Helvetica", # DiagrammeR's default
    quickdag.circles_fontsize = 5,
    quickdag.circles_fontcolor = "black",
    quickdag.pearl_pointsize = 0.02,
    quickdag.pearl_pointcolor = "black",
    quickdag.pearl_pointfill = "black",
    quickdag.pearl_linewidths = 0.2,
    quickdag.pearl_fontname = "Helvetica", # DiagrammeR's default
    quickdag.pearl_fontsize = 5,
    quickdag.pearl_fontcolor = "black"
  )
  options(opts)
}
