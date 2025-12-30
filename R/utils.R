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
    quickdag.base_rankdir = "LR",
    quickdag.base_layout = "dot",
    quickdag.base_shape = "plaintext",
    quickdag.base_nodepen = 0.5,
    quickdag.base_nodewidth = 0,
    quickdag.base_nodeheight = 0,
    quickdag.base_edgepen = 0.5,
    quickdag.base_linewidths = 0.2,
    quickdag.base_fontname = "Helvetica", # DiagrammeR's default
    quickdag.base_fontsize = 5,
    quickdag.base_fontcolor = "black",
    quickdag.circles_nodepen = 0.5,
    quickdag.circles_edgepen = 0.5,
    quickdag.circles_fontname = "Helvetica", # DiagrammeR's default
    quickdag.circles_fontsize = 5,
    quickdag.circles_fontcolor = "black",
    quickdag.pearl_pointsize = 0.02,
    quickdag.pearl_pointcolor = "black",
    quickdag.pearl_pointfill = "black",
    quickdag.pearl_edgepen = 0.2,
    quickdag.pearl_arrowsize = 0.2,
    quickdag.pearl_fontname = "Helvetica", # DiagrammeR's default
    quickdag.pearl_fontsize = 5,
    quickdag.pearl_fontcolor = "black"
  )
  options(opts)
}
