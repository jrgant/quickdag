#' Make, view, and embed beautiful diagrams.
#'
#' Save directed acyclic graphs (DAGs) generated in DiagrammeR and export them to pdf, png, eps, or svg format. This package is designed to make specification and production of DAGs and other diagrams as quick and painless as possible.
#'
#' @param edgelist A vector of edge relationships. Must be strictly organized (see example).
#' @param node.labs A character vector containing label names. Defaults to \code{NULL}.
#' @param node.aes.opts A list feeding aesthetic options for nodes to \code{DiagrammeR::node_aes()}. Defaults to empty list.
#' @param edge.aes.opts A list feeding aesthetic options for edges to \code{DiagrammeR::edge_aes()}. Defaults to empty list.
#' @param checks A logical switch indicating whether to print node and edge dataframes to the console. See NOTE below. Defaults to \code{TRUE}.
#'
#' @note
#' Leaving the \code{checks} option selected may be advisable to ensure labels and IDs have not been mismatched. By default, \code{qd_dag()} alphabetizes nodes included in \code{edgelist} and does the same for \code{node.labs} under a first assumption that labels will begin with the same letter as their corresponding \code{alpha.id, which may not always be the case.}
#'
#' Suggestions and bug reports welcome at \url{https://github.com/jrgant/quickDAG/issues}.
#'
#'
#' Packages used: DiagrammeR, stringr, purrr
#'
#' @examples
#' # Provide a list of edges, with nodes specified as letters.
#' # Do not list a node as a parent more than once.
#' # Function cannot currently accept the form: "A -> B -> C".
#' edges <- c("A -> { B C }",
#'            "B -> D")
#'
#' # make a DAG object and render the graph
#' g.obj <- qd_dag(edges)
#' render_graph(g.obj)
#'
#' @export qd_dag
#' @import DiagrammeR
#' @import stringr
#' @import purrr


# This function takes simple input, requiring only a vector of node-edge
#  relationships to produce a minimal graph object. Other inputs provide
#  additional functional and graphical features.
qd_dag <- function(edgelist, node.labs = NULL,
                   node.aes.opts = list(), edge.aes.opts = list(),
                   checks = TRUE) {

  # Identify Nodes --------------------------------------------------------
  ## extract unique nodes, sort in ascending order
  nodes <- sort(unique(unlist(str_extract_all(edgelist, pattern = "[:alnum:]"))))
  ## specify nodes with direct descendants (out = list)
  pa.nodes <- str_extract_all(edgelist, pattern = "^.{1}")
  ## specify nodes with direct ancestors (out = list)
  ch.nodes <- str_extract_all(edgelist, pattern = "(?<=\\>.{1,1000})[:alnum:]")


  # Create Node Dataframe -------------------------------------------------

  ## setup options list to feed to do.call('create_node_df')
  nd.opts.list <- node.aes.opts
  nd.opts.list$n <- length(nodes)
  nd.opts.list$label <- nodes

  ## create node dataframe with options
  ndf <- do.call("create_node_df", nd.opts.list)
  ndf$alpha.id <- nodes

  ## apply node labels if present
  if (!is.null(node.labs)) {
    ndf$label <- sort(node.labs)
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
