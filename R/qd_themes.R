#' Plot themes
#'
#' @description
#' Apply various pre-fabricated themes to diagrams.
#'
#' @param graph.obj A DAG object created by \code{qd_dag()}.
#' @param conditioned A character vector indicating which nodes are conditioned upon. The shape for these nodes will be set to "square".
#' @param theme A character string indicating the theme to use. Defaults to "base". Set to \code{NULL} to use GraphViz defaults.
#' @param font A character vector indicating the font family to use for node labels. Defaults to "serif".
#'
#' @import DiagrammeR
#' @importFrom dplyr data_frame
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate


#' @rdname qd_themes
#' @export qd_themes
# wrapper for theme selection
qd_themes <- function(graph.obj,
                      theme = "base",
                      ...) {

  select.theme <- c("base" = "theme_base")
  do.call(select.theme[theme], args = list(graph.obj = graph.obj, ...))
}

#' @rdname qd_themes
#' @export theme_base
# base theme (default)
theme_base <- function(graph.obj, conditioned = NULL, font = "serif") {
  graph_attrs <- data_frame(attr = c("rankdir", "layout"),
                            value = c("LR", "dot"),
                            attr_type = "graph")

  node_attrs <- data_frame(attr = c("shape", "penwidth", "fontname",
                                    "width", "height"),
                           value = c("plaintext", "0.5", font,
                                     "0", "0"),
                           attr_type = "node")

  edge_attrs <- data_frame(attr = c("arrowsize", "penwidth"),
                           value = c("0.4", "0.5"),
                           attr_type = "edge")

  graph.obj$global_attrs = bind_rows(graph_attrs, node_attrs, edge_attrs)

  if (!is.null(conditioned)) {

    default.shape <- with(graph.obj$global_attrs, value[attr == "shape"])
    default.minwd <- with(graph.obj$global_attrs, value[attr == "width"])
    default.minht <- with(graph.obj$global_attrs, value[attr == "height"])

    cd.nodes <- graph.obj %>%
      get_node_ids(conditions = alpha.id %in% conditioned)

    graph.obj <- graph.obj %>%
      # add default columns to node_df based on global_attrs
      set_node_attrs(node_attr = "shape", values = default.shape) %>%
      set_node_attrs(node_attr = "width", values = default.minwd) %>%
      set_node_attrs(node_attr = "height", values = default.minht) %>%
      # select conditioned nodes and update node aesthetics
      select_nodes_by_id(cd.nodes) %>%
      set_node_attrs_ws(node_attr = "shape",
                        value = "square") %>%
      set_node_attrs_ws(node_attr = "width",
                        value = "0") %>%
      set_node_attrs_ws(node_attr = "height",
                        value = "0") %>%
      clear_selection()
  }

  return(graph.obj)
}
