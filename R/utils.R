# Put some variables in the global environment to avoid R CMD CHECK notes
utils::globalVariables(c("alpha_id", "label", "start", "end"))

# Set global options
.onLoad <- function(libname, pkgname) {
  opts <- list(
    quickdag.check_dag         = TRUE,
    quickdag.embed             = FALSE,
    quickdag.swig_fixedsep     = "vlin",
    quickdag.swig_sepsize      = 15,
    quickdag.format_special    = FALSE,
    quickdag.theme             = "base",
    quickdag.verbose           = FALSE,
    quickdag.base_rankdir      = "LR",
    quickdag.base_layout       = "dot",
    quickdag.base_nodewidth    = 0,
    quickdag.base_shape        = "plaintext",
    quickdag.base_nodepen      = 0.5,
    quickdag.base_nodeheight   = 0,
    quickdag.base_edgepen      = 0.5,
    quickdag.base_arrowhead    = "normal",
    quickdag.base_arrowsize    = 0.4,
    quickdag.base_fontname     = "Helvetica", # DiagrammeR's default
    quickdag.base_fontsize     = 5,
    quickdag.base_fontcolor    = "black",
    quickdag.circles_nodepen   = NA,
    quickdag.circles_edgepen   = NA,
    quickdag.circles_fontname  = NA,
    quickdag.circles_fontsize  = NA,
    quickdag.circles_fontcolor = NA,
    quickdag.pearl_pointsize   = 0.02,
    quickdag.pearl_pointcolor  = "black",
    quickdag.pearl_pointfill   = "black",
    quickdag.pearl_edgepen     = NA,
    quickdag.pearl_arrowhead   = NA,
    quickdag.pearl_arrowsize   = 0.2,
    quickdag.pearl_fontname    = NA,
    quickdag.pearl_fontsize    = NA,
    quickdag.pearl_fontcolor   = NA
  )
  options(opts)
}

choose_option <- function(x) {
  themeopt <- stringr::str_remove(x, "quickdag\\.(circles|pearl)_")
  if (is.na(getOption(x))) {
    getOption(paste0("quickdag.base_", themeopt))
  } else {
    getOption(x)
  }
}
