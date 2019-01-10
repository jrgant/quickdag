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

  # identify relations between parents and children
  rel.l <- lapply(fixed.nodes, FUN = function(x) {
    pt.id <- get_node_ids(graph.obj, conditions = alpha.id == x)
    ch.id <- get_successors(graph.obj, node = pt.id)

    df <- data_frame(pt.id, pt.alpha = x, ch.id)
  })

  rel.df <- bind_rows(rel.l)

  # create label insert based on child's fixed parents
  unq.ch <- unique(rel.df$ch.id)
  slug.l <- lapply(unq.ch, FUN = function(x) {
    curr.ch <- x
    lab.slug <- with(rel.df,
                     paste(tolower(pt.alpha[ch.id == x]), collapse = ","))
    df <- data_frame(ch.id = curr.ch, lab.slug)
  })

  slug.df <- bind_rows(slug.l)

  # separators for fixed nodes
  sep.opts <- sep_opts()
  sep.choice <- sep.opts[fixed.sep]
  if (!fixed.sep %in% names(sep.opts)) sep.choice <- fixed.sep

  # update child label in node_df
  graph.obj$nodes_df <-
    graph.obj %>%
    get_node_df() %>%
    mutate(
      label = if_else(id %in% slug.df$ch.id,
                     paste0(alpha.id, "@^{<i>",
                            slug.df$lab.slug[match(id, slug.df$ch.id)], "</i>}"),
                     label),
      label = if_else(id %in% rel.df$pt.id,
                     paste0(label, sep.choice, "<i>",
                            tolower(alpha.id), "</i>",
                            "@_{ }"), # kludge to force DOT to render italics
                     label),
      fixed = if_else(id %in% rel.df$pt.id, TRUE, FALSE))

  return(graph.obj)

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

  dagedges <- with(graph.obj$edges_df, paste(from, "->", to))
  dagspec  <- paste("dag {",
                        paste(dagittyedges, collapse = " ; "),
                        "}")

  dagittydag   <- dagitty(dagittyspec)
  ancestors    <- ancestors(x = dagittydag,
                            v = curr.numid)

  # "By convention", dagitty returns the node of interest when returning
  #  ancestors. Drop curr.numid to leave it out for qd_swig().
  return(as.numeric(ancestors)[-curr.numid])
}



