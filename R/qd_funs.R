# This function takes the output produced by DiagrammeR::render_graph() and
#  saves the image to a given location in the specified format.
#  Preserves the ability to send arguments directly to render_graph().
qd_save <- function(graph, filename = NULL, filetype = "pdf", ...) {

  # File Format Match Table -----------------------------------------------
  fmt.opts <- c("png" = "rsvg_png",
                "eps" = "rsvg_ps",
                "pdf" = "rsvg_pdf",
                "svg" = "rsvg_svg")

  # Checks ----------------------------------------------------------------

  ## check for valid filetype
  if (!filetype %in% names(fmt.opts)) {
    stop(paste("Filetype not supported. Choose one of:",
               paste0(names(fmt.opts), collapse = ", ")))
  }

  ## check for filename
  if (is.null(filename)) {
    stop("Must specify the 'filename' option.")
  }


  # Graph Object ----------------------------------------------------------

  ## detect whether 'graph' = graph object or pre-rendered graph
  if (class(graph)[1] == "dgr_graph") {
    rendered.graph <- render_graph(graph, ...)
  } else if (class(graph)[1] == "grViz") {
    rendered.graph <- graph
  }

  # File Save -------------------------------------------------------------
  file.fmt <- match.arg(filetype, names(fmt.opts))
  raw.img <- charToRaw(export_svg(rendered.graph))

  fname <- paste(filename, file.fmt, sep = ".")
  out <- capture.output(
    do.call(fmt.opts[[file.fmt]],
            list(raw.img, file = fname))
  )
  return(out)
}


# Calls qd_save and embeds image in RMarkdown document
qd_embed <- function(...) {
  qd_save(...)
  knitr::include_graphics(fname)
}



# This function will take an edgelist in the form that qd_dag takes and feed
#  it to dagitty in order to identify adjustment sets. Can specify additional
#  dagitty arguments to adjustmentSets() as well.
qd_todagitty <- function(edgelist, diagram_type = "dag", showplot = FALSE,
                         adjustmentvars, ...) {

  require(dagitty)

  dagitty.obj <- dagitty(paste(diagram_type, "{",
                               paste(edgelist, collapse = "; "),
                               "}"),
                         layout = TRUE)

  ## optional to show dagitty plot
  if (showplot) {
    plot(dagitty.obj)
  }

  ## use dagitty's algorithm to identify adjustment sets
  if (adjustmentvars) {
    adjustmentSets(dagitty.obj, ...)
  }

}
