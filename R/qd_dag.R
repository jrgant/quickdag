#' Generate a graph object
#'
#' @description
#' Provide simple syntax specifying paths between nodes to generate a graph object.
#'
#' @param edgelist A vector of node-edge relationships.
#' @param node_labs A named character vector containing label names. Defaults
#'   to `NULL`.
#' @param node_aes_opts A list feeding aesthetic options for nodes to
#'   [DiagrammeR::node_aes()]. Defaults to empty list.
#' @param edge_aes_opts A list feeding aesthetic options for edges to
#'   [DiagrammeR::edge_aes()]. Defaults to empty list.
#' @param format_special Render numeric elements in an alphanumeric `alpha_id` as
#'   subcripts. Defaults to `TRUE`.
#' @param verbose Indicate whether to print node and edge dataframes to the console.
#'   See Details below. Defaults to `TRUE`.
#' @param check_dag Logical. Check whether the graph conforms to the rules of DAGs.
#'   Defaults to `TRUE`.
#' @param theme Choose theme for plot output. Defaults to `"base"`. Setting theme to
#'   `NULL` will use DiagrammeR's `NULL` attribute theme.
#' @param ... Pass optional `conditioned` argument to [qd_themes()].
#'
#' @details
#' Leaving the `verbose` set to `TRUE` may be advisable to ensure labels and IDs have
#' not been mismatched. By default, [qd_dag()] alphabetizes nodes included in `edgelist`
#' and does the same for `node_labs` under a first assumption that labels will begin with
#' the same letter as their corresponding `alpha_id`, which may not always be the case.
#' @export
#' @examples
#' # Provide a list of edges, with nodes specified as letters or single words.
#' edges <- c("A -> { B C } <- L",
#'            "B -> C")
#'
#' # make a DAG object and render the graph using the default theme
#' g.obj <- qd_dag(edges)
#' DiagrammeR::render_graph(g.obj)
#'
#' # Pass labels and aesthetic options for nodes or edges
#' g.obj2 <- qd_dag(edges,
#'                  node_labs = c("A" = "Alcohol",
#'                                "B" = "BP",
#'                                "C" = "CVD",
#'                                "L" = "State"),
#'                  node_aes_opts = list(shape = "plaintext",
#'                                       fillcolor = "none",
#'                                       color = "black"),
#'                  edge_aes_opts = list(arrowsize = 0.5,
#'                                       color = "gray"),
#'                  theme = NULL)
#' DiagrammeR::render_graph(g.obj2)
#'
qd_dag <- function(edgelist, node_labs = NULL,
                   node_aes_opts = list(), edge_aes_opts = list(),
                   format_special = TRUE,
                   verbose = FALSE, check_dag = TRUE, theme = "base", ...) {

  # Identify Nodes --------------------------------------------------------
  ## extract unique nodes, sort in ascending order
  nodes <- parse_nodes(edgelist)

  # Create Node Dataframe -------------------------------------------------

  ## setup options list to feed to do.call('create_node_df')
  nd_opts_list <- node_aes_opts
  nd_opts_list$n <- length(nodes)
  nd_opts_list$label <- nodes

  ## create node dataframe with options
  ndf <- do.call(DiagrammeR::create_node_df, nd_opts_list)
  ndf$alpha_id <- nodes

  ## apply node labels if present
  if (!is.null(node_labs)) {
    ndf <- ndf |>
      dplyr::mutate(
        label = dplyr::if_else(
          alpha_id %in% names(node_labs),
          unname(node_labs[alpha_id]),
          label
        )
      )
  }

  ## check for and format special labels
  if (format_special == TRUE) {
    ndf <- ndf |>
      dplyr::mutate(
        label = dplyr::if_else(
          stringr::str_detect(alpha_id, "^[:alpha:]{1}[0-9]+"),
          paste0(
            stringr::str_match(alpha_id, "^[:alpha:]{1}"), "@_{",
            stringr::str_match(alpha_id, "[0-9]+"), "}"
          ),
          label
        )
      )
  }
  
  # Create Edge Dataframe -------------------------------------------------
  edges <- parse_edges(edgelist)

  ## match alphabetical node ID to numeric ID
  pa_vec_num <- ndf$id[match(edges$from_alpha, ndf$alpha_id)]
  ch_vec_num <- ndf$id[match(edges$to_alpha, ndf$alpha_id)]

  ## set up edge dataframe options list
  ed_opts_list <- edge_aes_opts
  ed_opts_list$from <- pa_vec_num
  ed_opts_list$to <- ch_vec_num
  ed_opts_list$from_alpha <- edges$from_alpha
  ed_opts_list$to_alpha <- edges$to_alpha

  ## create edge dataframe with options
  edf <- do.call(DiagrammeR::create_edge_df, ed_opts_list)

  # Output Graph Object -----------------------------------------------------
  graph <- DiagrammeR::create_graph(nodes_df = ndf,
                                    edges_df = edf,
                                    attr_theme = NULL)

  # Checks ------------------------------------------------------------------

  ## check to see if graph is a DAG
  if (check_dag == TRUE) {
    if (!DiagrammeR::is_graph_dag(graph)) {
      warning("Your graph appears to break some of the rules of ",
              "directed acylic graphs. ",
              "To turn off this warning, set 'check.dag' option to FALSE. \n\n")
    } else {
      message("\n\nCHECKED: The diagram is a DAG. \n")
    }
  }

  ## print the node and edges dataframes to console (optional)
  ## defaults to TRUE
  if (verbose == TRUE) {

    sep_length <- 73

    # preamble
    message(rep("-", sep_length))
    message("Make sure everything is matched up properly! \n",
            "To stop printing data to the console, set 'verbose' to FALSE.")
    message(rep("-", sep_length), "\n")

    # dataframes
    dots <- paste(rep(".", sep_length / 3), collapse = "")
    cat(dots, "NODE DATAFRAME", dots, "\n\n")
    graph |> DiagrammeR::get_node_df() |> print()
    cat("\n")
    cat(dots, "EDGE DATAFRAME", dots, "\n\n")
    graph |> DiagrammeR::get_edge_df() |> print()
  }

  ## returns DiagrammeR's graph object to store node and edge dataframes
  ## and attributes
  if (!is.null(theme)) {
    if (length(node_aes_opts) > 0 || length(edge_aes_opts) > 0) {
      warning(
        "Node and/or edge aesthetics are currently being applied ",
        "via both node_aes_opts or edge_aes_opts AND a diagram theme. ",
        "Using both methods to set aesthetics may produce unexpected results."
      )
    }

    themed_graph <- graph |> qd_themes(theme = theme, ...)
    return(themed_graph)

  } else {
    return(graph)
  }

}
