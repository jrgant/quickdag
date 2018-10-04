#' Generate a graph object
#'
#' @description
#' Provide simple syntax specifying paths between nodes to generate a graph object.
#'
#' @param edgelist A vector of edge relationships. Must be strictly organized (see example for format).
#' @param node.labs A character vector containing label names. Defaults to \code{NULL}.
#' @param node.aes.opts A list feeding aesthetic options for nodes to \code{DiagrammeR::node_aes()}. Defaults to empty list. See \code{?node_aes} to view available parameters.
#' @param edge.aes.opts A list feeding aesthetic options for edges to \code{DiagrammeR::edge_aes()}. Defaults to empty list. See \code{?edge_aes} to view available parameters.
#' @param verbose Indicate whether to print node and edge dataframes to the console. See NOTE below. Defaults to \code{TRUE}.
#' @param check.dag Logical. Check whether the graph conforms to the rules of DAGs. Defaults to \code{TRUE}.
#'
#' @note
#' Leaving the \code{checks} option selected may be advisable to ensure labels and IDs have not been mismatched. By default, \code{qd_dag()} alphabetizes nodes included in \code{edgelist} and does the same for \code{node.labs} under a first assumption that labels will begin with the same letter as their corresponding \code{alpha.id}, which may not always be the case.
#'
#' Suggestions and bug reports welcome at \url{https://github.com/jrgant/quickDAG/issues}.
#'
#'
#' Packages used: DiagrammeR, stringr, purrr
#'
#' @examples
#' # Provide a list of edges, with nodes specified as letters.
#' # Do not list a node as a parent more than once.
#' # Each line should contain a single edge character '->'.
#' edges <- c("A -> { B C }",
#'            "B -> C")
#'
#' # make a DAG object and render the graph
#' g.obj <- qd_dag(edges)
#' render_graph(g.obj)
#'
#' # Pass labels and aesthetic options for nodes or edges
#' g.obj2 <- qd_dag(edges,
#'                  node.labs = c("Alcohol", "BP", "CVD"),
#'                  node.aes.opts = list(shape = "plaintext",
#'                                       fillcolor = "none",
#'                                       color = "black"),
#'                  edge.aes.opts = list(arrowsize = 0.5,
#'                                       color = "gray"))
#' render_graph(g.obj2)
#'
#' @export qd_dag
#' @import DiagrammeR
#' @import stringr
#' @importFrom purrr map2



qd_dag <- function(edgelist, node.labs = NULL,
                   node.aes.opts = list(), edge.aes.opts = list(),
                   verbose = TRUE, check.dag = TRUE) {

  # Identify Nodes --------------------------------------------------------
  ## extract unique nodes, sort in ascending order
  nodes <- sort(unique(unlist(str_extract_all(edgelist, pattern = "[:alnum:]"))))
  ## specify nodes with direct descendants (out = list)
  pa.nodes <- str_extract_all(edgelist, pattern = "^.{1}")
  ## specify nodes with direct ancestors (out = list)
  ch.nodes <- str_extract_all(edgelist, pattern = "(?<=\\>.{0,1000})[:alnum:]")


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

    # alpha label prefix
    alpha.prefix <- "^[A-Z]{1}"
    alpha.period <- paste0(alpha.prefix, "\\.")

    # // label finding
    # if all node.labs conform to the format "A.label", extract and save "label"
    # to node dataframe; else save full string to node dataframe
    # paste0() includes the period
    if (!0 %in% grepl(alpha.period, node.labs)) {
      ndf$label[match(ndf$alpha.id, str_extract(node.labs, alpha.prefix))] <-
        str_replace(string = node.labs,
                    pattern = alpha.period,
                    replacement = "")
    } else {
      ndf$label <- sort(node.labs)
    }
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


# Checks ------------------------------------------------------------------

  ## check to see if graph is a DAG
  if (check.dag) {
    if (!is_graph_dag(graph)) {
      warning("Your graph appears to break some of the rules of ",
              "directed acylic graphs. ",
              "To turn off this warning, set 'check.dag' option to FALSE. \n\n")
    } else {
      message("\n\nCHECKED: The diagram is a DAG. \n")
    }
  }

  ## print the node and edges dataframes to console (optional)
  ## defaults to TRUE
  if (verbose) {

    sep.length <- 73

    # preamble
    message(rep("-", sep.length))
    message("Make sure everything is matched up properly! \n",
            "To stop printing data to the console, set 'verbose' to FALSE.")
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
