#' Graph operations
#'
#' @param graph_obj A `quickdag` object output by [qd_dag()] or [qd_swig()].
#' @param alpha_ids A vector of alphanumeric node IDs upon which to operate.
#' @param ... Passed to [DiagrammeR::set_node_attrs()] or [DiagrammeR::set_edge_attrs()]
#'   argument `node_attr` or `edge_attr`, respectively.
#' @param edge_attr Passed to  argument of the same name.
#' @param values Passed to [DiagrammeR::set_node_attrs()] or
#'   [DiagrammeR::set_edge_attrs()] argument of the same name.
#' @param from_alpha A vector of alphanumeric source node IDs.
#' @param to_alpha A vector of alphanumeric destination node IDs.
#' @param set_op Passed to the `set_op` argument of [DiagrammeR::select_nodes_by_id()]
#'   or [DiagrammeR::select_edges_by_node_id()]. Defaults to "union".
#'
#' @rdname graph_operations
#' @export
qd_set_node_attrs <- function(graph_obj, ..., values, alpha_ids) {
  numids <- get_numids(graph_obj, alpha_ids)
  graph_out <- DiagrammeR::set_node_attrs(graph_obj,
                                          ...,
                                          values = values,
                                          nodes = numids)
  graph_out
}

#' @rdname graph_operations
#' @export
qd_set_edge_attrs <- function(graph_obj, ..., values,
                              from_alpha = NULL, to_alpha = NULL) {
  from_numids <- NULL
  to_numids <- NULL
  if (!is.null(from_alpha)) {
    from_numids <- get_numids(graph_obj, from_alpha)
  }
  if (!is.null(to_alpha)) {
    to_numids <- get_numids(graph_obj, to_alpha)
  }
  graph_out <- DiagrammeR::set_edge_attrs(graph_obj,
                                          ...,
                                          values = values,
                                          from = from_numids,
                                          to = to_numids)
  graph_out
}

#' @rdname graph_operations
#' @export
select_nodes_by_alpha_id <- function(graph_obj, alpha_ids, set_op = "union") {
  numids <- get_numids(graph_obj, alpha_ids)
  graph_out <- DiagrammeR::select_nodes_by_id(graph_obj,
                                              nodes = numids,
                                              set_op = set_op)
  graph_out
}

#' @rdname graph_operations
#' @export
qd_select_nodes <- select_nodes_by_alpha_id

#' @rdname graph_operations
#' @export
select_edges_by_node_alpha_id <- function(graph_obj, alpha_ids, set_op = "union") {
  numids <- get_numids(graph_obj, alpha_ids)
  graph_out <- DiagrammeR::select_edges_by_node_id(graph_obj,
                                                   nodes = numids,
                                                   set_op = set_op)
  graph_out
}

#' @rdname graph_operations
#' @export
qd_select_edges <- select_edges_by_node_alpha_id

#' Retrieves numeric IDs given alphanumeric IDs
#' @inheritParams qd_set_node_attrs
get_numids <- function(graph_obj, alpha_ids) {
  graph_obj$nodes_df |>
    dplyr::filter(alpha_id %in% alpha_ids) |>
    dplyr::pull(id)
}
