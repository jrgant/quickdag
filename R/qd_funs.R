# This function takes simple input, requiring only a vector of node-edge
#  relationships to produce a minimal graph object. Other inputs provide
#  additional functional and graphical features.
qd_dag <- function(edgelist, id.type = "alpha", node.labs = NULL,
                   node.aes.opts = list(), edge.aes.opts = list(),
                   checks = TRUE) {

  # Identify Nodes --------------------------------------------------------
  ## extract unique nodes, sort in ascending order
  nodes <- sort(unique(unlist(str_extract_all(edges, pattern = "[:alnum:]"))))
  ## specify nodes with direct descendants (out = list)
  pa.nodes <- str_extract_all(edges, pattern = "^.{1}")
  ## specify nodes with direct ancestors (out = list)
  ch.nodes <- str_extract_all(edges, pattern = "(?<=\\>.{1,1000})[:alnum:]")


  # Create Node Dataframe -------------------------------------------------

  ## setup options list to feed to do.call('create_node_df')
  nd.opts.list <- node.aes.opts
  nd.opts.list$n <- length(nodes)
  nd.opts.list$label <- nodes

  ## create node dataframe with options
  ndf <- do.call("create_node_df", nd.opts.list)

  ## replace numeric ids with letters if option 'alpha' is selected
  if (id.type == "alpha") {
    if (!is.null(node.labs)) {
      ndf$label <- sort(node.labs)
    }
    ndf$alpha.id <- nodes
  }


  # Create Edge Dataframe -------------------------------------------------
  ## loop through pa.nodes and ch.nodes to match parents with children
  ## and create a parent entry for each child
  nodematches <- map2(.x = pa.nodes, .y = ch.nodes,
                      .f = function(x, y) rep(x, length(y)))
  pa.vec.alpha <- unlist(nodematches)

  ## match alphabetical node ID to numeric ID
  pa.vec.num <- ndf$id[match(pa.vec.alpha, ndf$alpha.id)]
  ch.vec.num <- ndf$id[match(unlist(ch.nodes), ndf$alpha.id)]

  ## set up edge dataframe options list
  ed.opts.list <- edge.aes.opts
  ed.opts.list$from <- pa.vec.num
  ed.opts.list$to <- ch.vec.num

  ## create edge dataframe with options
  edf <- do.call("create_edge_df", ed.opts.list)

  # Output Graph Object -----------------------------------------------------
  graph <- create_graph(nodes_df = ndf,
                        edges_df = edf)

  ## if user wants checks, print the node and edges dataframes to console
  ## defaults to TRUE
  if (checks) {

    sep.length <- 73

    # preamble
    message(rep("-", sep.length))
    message("Make sure everything is matched up properly!")
    message(paste("To stop printing dataframes to the console,",
                  "set 'checks' option to FALSE."))
    message(rep("-", sep.length), "\n")

    # dataframes
    dots <- paste(rep(".", sep.length / 3), collapse = "")
    cat(dots, "NODE DATAFRAME", dots, "\n\n")
    graph %>% get_node_df() %>% print()
    cat("\n")
    cat(dots, "EDGE DATAFRAME", dots, "\n\n")
    graph %>% get_edge_df() %>% print()
  }

  ## returns DiagrammeR's graph object to store node and edge dataframes
  ## and attributes
  return(graph)
}




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
