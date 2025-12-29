#' Diagram themes
#'
#' @description
#' Themes and theming utilies.
#'
#' @param graph_obj A DAG object created by [qd_dag()].
#' @param theme A character string indicating the theme to use. Defaults to "base".
#'   Set to `NULL` to use GraphViz defaults.
#' @param conditioned A character vector indicating which nodes are conditioned upon.
#'   The shape for these nodes will be set to "rectangle".
#' @param linewidths Size (in points) of edges and node outlines. By default, controlled
#'   by package options specific to each theme, each of which defaults to 0.2.
#' @param fontname Font name for text elements of the graph. By default, controlled by
#'   package options specific to each theme, each of which defaults to "Helvetica". This
#'   is the default in `DiagrammeR`.
#' @param fontsize Size of text (in points). By default, controlled by package options
#'   specific to each theme, each of which defaults to 5.
#' @param fontcolor Text color. By default, controlled by package options
#'   specific to each theme, each of which defaults to "black".
#' @param pointsize Size of "point" shape (in points). Applies to "pearl" theme only and
#'   defaults to 0.02.
#' @param pointcolor Border color for the "point" shape. Applies to "pearl" theme only
#'   and defaults to "black".
#' @param pointfill Fill color for the "point" shape. Applies to "pearl" theme only
#'   and defaults to "black".

#' @rdname qd_themes
#' @export
# wrapper for theme selection
qd_themes <- function(graph_obj, theme, conditioned = NULL) {

  select_theme <- c(
    "base"    = "theme_qd_base",
    "circles" = "theme_qd_circles",
    "pearl"   = "theme_qd_pearl"
  )

  if (!theme %in% names(select_theme)) {
    stop("`theme` must be one of: ", paste(names(select_theme), collapse = ", "))
  }

  graph_obj <- graph_obj |> cleanup_existing_theme()
  graph_obj$theme <- theme

  if (!is.null(conditioned)) {
    graph_obj$conditioned <- conditioned
  }

  do.call(select_theme[theme],
          args = list(graph_obj = graph_obj, conditioned = conditioned))
}

#' @rdname qd_themes
#' @export
theme_qd_base <- function(graph_obj,
                          linewidths = getOption("quickdag.base_linewidths"),
                          fontname   = getOption("quickdag.base_fontname"),
                          fontsize   = getOption("quickdag.base_fontsize"),
                          fontcolor  = getOption("quickdag.base_fontcolor"),
                          conditioned = NULL) {

  graph_obj <- graph_obj |> cleanup_existing_theme()

  graph_attrs <- tibble::tibble(
    attr = c("rankdir", "layout"),
    value = c("LR", "dot"),
    attr_type = "graph"
  )

  node_attrs  <- tibble::tibble(
    attr  = c("shape", "penwidth", "fontname", "width", "height"),
    value = c("plaintext", "0.5", fontname, "0", "0"),
    attr_type = "node"
  )

  edge_attrs  <- tibble::tibble(
    attr = c("arrowsize", "penwidth"),
    value = c("0.4", "0.5"),
    attr_type = "edge"
  )

  graph_obj$global_attrs <- dplyr::bind_rows(graph_attrs, node_attrs, edge_attrs)

  graph_obj <- graph_obj |> get_conditioned_nodes(conditioned = conditioned)

  graph_obj$theme <- "base"
  graph_obj
}

#' @rdname qd_themes
#' @export
theme_qd_circles <- function(graph_obj,
                             linewidths = getOption("quickdag.circles_linewidths"),
                             fontname   = getOption("quickdag.circles_fontname"),
                             fontsize   = getOption("quickdag.circles_fontsize"),
                             fontcolor  = getOption("quickdag.circles_fontcolor"),
                             conditioned = NULL) {

  # set base theme
  graph_obj <- graph_obj |> theme_qd_base()

  # tweak base theme
  graph_obj <- graph_obj |>
    DiagrammeR::add_global_graph_attrs("shape", "circle", "node")

  graph_obj <- graph_obj |> get_conditioned_nodes(conditioned = conditioned)

  graph_obj$theme <- "circles"
  graph_obj
}

#' @rdname qd_themes
#' @export
theme_qd_pearl <- function(graph_obj,
                           pointsize  = getOption("quickdag.pearl_pointsize"),
                           pointcolor = getOption("quickdag.pearl_pointcolor"),
                           pointfill  = getOption("quickdag.pearl_pointfill"),
                           linewidths = getOption("quickdag.pearl_linewidths"),
                           fontname   = getOption("quickdag.pearl_fontname"),
                           fontsize   = getOption("quickdag.pearl_fontsize"),
                           fontcolor  = getOption("quickdag.pearl_fontcolor"),
                           conditioned = NULL) {
  # set base theme
  graph_obj <- graph_obj |> theme_qd_base()

  # tweak base theme
  graph_obj <- graph_obj |>
    # node attribute tweaks
    DiagrammeR::add_global_graph_attrs("shape",        "point",  "node") |>
    DiagrammeR::add_global_graph_attrs("style",       "filled",  "node") |>
    DiagrammeR::add_global_graph_attrs("color",      pointcolor, "node") |>
    DiagrammeR::add_global_graph_attrs("width",      pointsize,  "node") |>
    DiagrammeR::add_global_graph_attrs("height",     pointsize,  "node") |>
    DiagrammeR::add_global_graph_attrs("fixedsize",       TRUE,  "node") |>
    DiagrammeR::add_global_graph_attrs("fontsize",    fontsize,  "node") |>
    # edge attribute tweaks
    DiagrammeR::add_global_graph_attrs("penwidth",  linewidths,  "edge") |>
    DiagrammeR::add_global_graph_attrs("arrowsize", linewidths,  "edge")

  ## Necessary because fillcolor via global attributes appears to be broken
  ## in DiagrammeR
  graph_obj$nodes_df$fillcolor <- pointfill

  ## Add and style external labels
  graph_obj$nodes_df$xlabel    <- graph_obj$nodes_df$label
  graph_obj$nodes_df$fontcolor <- fontcolor

  graph_obj <- graph_obj |> get_conditioned_nodes(conditioned = conditioned)
  graph_obj$nodes_df$label <- "" # Hide internal node labels

  graph_obj$theme <- "pearl"
  graph_obj
}

#' @rdname qd_themes
#' @export
get_conditioned_nodes <- function(graph_obj, conditioned = NULL) {
  if (!is.null(conditioned)) {

    default_shape <- with(graph_obj$global_attrs, value[attr == "shape"])
    default_minwd <- with(graph_obj$global_attrs, value[attr == "width"])
    default_minht <- with(graph_obj$global_attrs, value[attr == "height"])

    cd_nodes <- graph_obj |>
      DiagrammeR::get_node_ids(conditions = alpha_id %in% conditioned)

    graph_obj <- graph_obj |>
      # add default columns to node_df based on global_attrs
      DiagrammeR::set_node_attrs("shape",  default_shape) |>
      DiagrammeR::set_node_attrs("width",  default_minwd) |>
      DiagrammeR::set_node_attrs("height", default_minht) |>
      # select conditioned nodes and update node aesthetics
      DiagrammeR::select_nodes_by_id(cd_nodes) |>
      DiagrammeR::set_node_attrs_ws("shape",  "rectangle") |>
      DiagrammeR::set_node_attrs_ws("width",  "0") |>
      DiagrammeR::set_node_attrs_ws("height", "0") |>
      DiagrammeR::clear_selection()
  }
  graph_obj$conditioned <- conditioned
  graph_obj
}

#' @rdname qd_themes
#' @export
cleanup_existing_theme <- function(graph_obj) {
  graph_obj$nodes_df <- graph_obj$nodes_df[, c("id", "type", "label", "alpha_id")]
  ## Used for the side effect of returning an empty global_attrs table in the
  ## appropriate format
  graph_obj$global_attrs <- qd_dag("A", theme = NULL, check_dag = FALSE)$global_attrs
  graph_obj
}
