#' Diagram themes
#'
#' @description
#' Apply various pre-fabricated themes to diagrams.
#'
#' @param graph.obj A DAG object created by \code{qd_dag()}.
#' @param conditioned A character vector indicating which nodes are conditioned upon. The shape for these nodes will be set to "square".
#' @param theme A character string indicating the theme to use. Defaults to "base". Set to \code{NULL} to use GraphViz defaults.
#' @param font A character vector indicating the font family to use for node labels. Defaults to "serif".
#' @param ... Pass arguments to theme call (e.g., \code{theme_base()}), such as \code{conditioned} or \code{font}
#'
#' @import DiagrammeR

#' @rdname qd_themes
#' @export qd_themes
# wrapper for theme selection
qd_themes <- function(graph.obj, theme, ...) {

  select.theme <- c(
    "base" = "theme_base",
    "circles" = "theme_circles",
    "pearl" = "theme_pearl"
    )

  do.call(select.theme[theme],
          args = list(graph.obj = graph.obj, ...))
}

#' @rdname qd_themes
#' @export theme_base
theme_base <- function(graph.obj, font = "serif", ...) {

  graph_attrs <- tibble::tibble(
    attr = c("rankdir", "layout"),
    value = c("LR", "dot"),
    attr_type = "graph"
    )

  node_attrs  <- tibble::tibble(
    attr  = c("shape", "penwidth", "fontname", "width", "height"),
    value = c("plaintext", "0.5", font, "0", "0"),
    attr_type = "node"
    )

  edge_attrs  <- tibble::tibble(
    attr = c("arrowsize", "penwidth"),
    value = c("0.4", "0.5"),
    attr_type = "edge"
    )

  graph.obj$global_attrs <- dplyr::bind_rows(graph_attrs, node_attrs, edge_attrs)


  graph.obj <- graph.obj %>% get_conditioned_nodes(...)
  graph.obj
}

#' @rdname qd_themes
#' @export theme_circles
theme_circles <- function(graph.obj,
                          font = "serif", ...) {
  # set base theme
  graph.obj <- graph.obj %>% theme_base()

  # tweak base theme
  graph.obj <- graph.obj %>%
    add_global_graph_attrs("shape", "circle", "node")

  graph.obj <- graph.obj %>% get_conditioned_nodes(...)
  graph.obj
}

#' @rdname qd_themes
#' @export theme_dots
theme_dots <- function(graph.obj, font = "serif", ...) {
  # set base theme
  graph.obj <- graph.obj %>% theme_base()

  # tweak base theme
  graph.obj <- graph.obj %>%
    # node attribute tweaks
    add_global_graph_attrs("shape", "point", "node") %>%
    add_global_graph_attrs("width", 0.2, "node") %>%
    add_global_graph_attrs("height", 0.2, "node") %>%
    # edge attribute tweaks
    add_global_graph_attrs("penwidth", 0.2, "edge") %>%
    add_global_graph_attrs("arrowsize", 0.2, "edge")

  if (exists("conditioned")) {
    messaging::emit_message("This theme does not allow for conditioned nodes.")
  }

  graph.obj
}

#' @rdname qd_themes
#' @export get_conditioned_nodes
get_conditioned_nodes <- function(graph.obj, conditioned = NULL) {
  if (!is.null(conditioned)) {

    default.shape <- with(graph.obj$global_attrs, value[attr == "shape"])
    default.minwd <- with(graph.obj$global_attrs, value[attr == "width"])
    default.minht <- with(graph.obj$global_attrs, value[attr == "height"])

    cd.nodes <- graph.obj %>%
      get_node_ids(conditions = alpha.id %in% conditioned)

    graph.obj <- graph.obj %>%
      # add default columns to node_df based on global_attrs
      set_node_attrs("shape",  default.shape) %>%
      set_node_attrs("width",  default.minwd) %>%
      set_node_attrs("height", default.minht) %>%
      # select conditioned nodes and update node aesthetics
      select_nodes_by_id(cd.nodes) %>%
      set_node_attrs_ws("shape",  "square") %>%
      set_node_attrs_ws("width",  "0") %>%
      set_node_attrs_ws("height", "0") %>%
      clear_selection()
  }
  graph.obj
}
