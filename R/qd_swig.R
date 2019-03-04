#' Generate a single word intervention graph (SWIG) template
#'
#' @description
#' Provide simple syntax specifying paths between nodes to generate a graph object.
#'
#' @param graph.obj A DAG object created by \code{qd_dag()}.
#' @param fixed.nodes A vector containing the nodes to be intervened upon.
#' @param fixed.sep A character string indicating which character to use as a separator in fixed nodes. Defaults to "vlin". Run \code{sep_opts()} for available options.
#'
#'
#' @examples
#' # Provide a DAG object and a list of nodes to be fixed
#' edges <- c("A -> Y",
#'            "L -> { A Y }")
#'
#' qd_dag(edges) %>%
#'    qd_swig(fixed.nodes = "A") %>%
#'    render_graph()
#'
#' @export qd_swig
#' @import DiagrammeR
#' @importFrom dplyr data_frame bind_rows mutate if_else

qd_swig <- function(graph.obj, fixed.nodes, fixed.sep = "vlin") {
  # graph.obj = graph
  # fixed = alpha IDs for fixed nodes
  ndf <- get_node_df(graph.obj)
  ndf$fixed <- with(ndf, ifelse(alpha.id %in% fixed.nodes, TRUE, FALSE))

  fx.pathlist <-
    map(
      .x = set_names(ndf$alpha.id, ndf$alpha.id),
      .f = function(x) {
        curr.id  <- with(ndf, id[alpha.id == x])
        # each path will include current node id
        ancestors <-
          get_paths(graph.obj, to = curr.id) %>%
          map(~ .x[.x != curr.id])


        fx.nodes <-
          ancestors %>%
          purrr::map(function(x) {
            detect(x, function(y) y %in% with(ndf, id[fixed]), .dir = "backward")
          })
        unique(unlist(fx.nodes))
      })

  fx.ancestors <- discard(fx.pathlist, function(x) is.null(x))

  lab <-
    fx.ancestors %>%
    map_chr(~ with(ndf, paste0(tolower(alpha.id[id %in% .x]), collapse = ",")))

  graph.obj$nodes_df <-
    ndf %>%
    mutate(
      label = paste0(alpha.id,
                     if_else(alpha.id %in% names(lab), paste0("@^{<i>", lab[alpha.id], "</i>}"), ""),
                     if_else(fixed, paste0("&nbsp;", sep_opts()[fixed.sep], "<i>", tolower(alpha.id), "</i> @_{ }"), ""))
    )
  graph.obj
}

#' Find the ancestors of a given node
#'
#' @param graph.obj A quickDAG (DiagrammeR) graph object.
#' @param node.alpha The alphabetical ID of the node for which to gather the ancestors.
#'
#' @note
#' The \code{get_ancestors} function returns the numeric node IDs corresponding to the alphabetical ID provided in
#' \code{node.alpha}. It reformats the graph and passes it to \code{dagitty} for this purpose.
#'
#' @examples
#'
#' ## Not run:
#' dag <- qd_dag(c("A -> Y",
#'                 "X -> A",
#'                 "L -> { X A Y }"))
#'
#' get_ancestors(dag, "A")
#'
#' @export get_ancestors
#' @rdname qd_swig
#' @importFrom magrittr %>%
#' @importFrom dagitty ancestors dagitty
#' @importFrom messaging emit_error

get_ancestors <- function(graph.obj, node.alpha = NULL) {

  if (is.null(node.alpha)) {
    emit_error("Provide a the alphabet ID")
  }

  curr.numid <- with(graph.obj$nodes_df, id[alpha.id %in% node.alpha])
  #return(curr.numid)

  dag.edges <- with(graph.obj$edges_df, paste(from, "->", to))
  dag.spec  <- paste("dag {",
                        paste(dag.edges, collapse = " ; "),
                        "}")

  dagitty.dag   <- dagitty(dag.spec)
  ancestors     <- ancestors(x = dagitty.dag,
                             v = curr.numid)
  ancestors.num <- as.numeric(ancestors)

  # "By convention", dagitty returns the node of interest when returning
  #  ancestors. Drop curr.numid to leave it out for qd_swig().
  return(ancestors.num[ancestors.num != curr.numid])
}



