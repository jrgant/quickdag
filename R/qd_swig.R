#' Generate a single-world intervention graph or template (SWI(G/T))
#'
#' @description
#' Take a DAG graph object and, in the simplest case, create a single-world
#'   intervention template corresponding to a world in which the fixed_nodes are
#'   set to a given value. Alternatively, tell qd_swig which values fixed_nodes
#'   will be set to.
#'
#' @param graph_obj A DAG object created by \code{qd_dag()}.
#' @param fixed_nodes A vector containing the nodes to be intervened upon.
#' @param custom_values A named vector containing alternative labels identifying
#'   explicit values for fixed_nodes (e.g., a = 1).
#' @param fixed_sep A character string indicating which character to use as a
#'   separator in fixed_nodes. Defaults to "vlin". Run \code{sep_opts(T)} for
#'   available options.
#' @param sep_point_size A numerical value specifying the point size for fixed
#'   node separators.
#'
#' @examples
#' # Provide a DAG object and a list of nodes to be fixed
#' edges <- c("A -> Y",
#'            "L -> { A Y }")
#'
#' dag  <- qd_dag(edges)
#'
#' swig <- dag %>%
#'         qd_swig(fixed_nodes = "A",
#'                 custom_values = c("A" = "1"))
#'
#' swig %>% render_graph()
#'
#' @export qd_swig
#' @import DiagrammeR
#' @import purrr
#' @importFrom dplyr bind_rows mutate case_when

qd_swig <- function(graph_obj,
                    fixed_nodes,
                    custom_values = NULL,
                    fixed_sep = "vlin",
                    sep_point_size = 15) {
  # graph_obj = graph
  # fixed = alpha_ids for fixed_nodes
  ndf <- get_node_df(graph_obj)
  ndf$fixed <- with(ndf, ifelse(alpha_id %in% fixed_nodes, TRUE, FALSE))

  fx_pathlist <-
    map(
      .x = set_names(ndf$alpha_id, ndf$alpha_id),
      .f = function(x) {
        curr_id  <- with(ndf, id[alpha_id == x])
        # each path will include current node id by default
        # map() set up to drop the destination node
        ancestors <-
          get_paths(graph_obj, to = curr_id) %>%
          map(~ .x[.x != curr_id])

        fx_nodes <-
          ancestors %>%
          purrr::map(function(x) {
            purrr::detect(
              x,
              function(y) y %in% with(ndf, id[fixed]), .dir = "backward"
            )
          })
        unique(unlist(fx_nodes))
      })

  fx_ancestors <- discard(fx_pathlist, function(x) is.null(x))

  # create labels for those in custom_values
  if (is.null(custom_values)) {
    lab <-
      fx_ancestors %>%
      map_chr(~ with(ndf, paste0(tolower(label[id %in% .x]), collapse = ",")))
  } else {
    lab <-
      fx_ancestors %>%
      map_chr(~ with(ndf, paste0(tolower(label[id %in% .x]), "=",
                              custom_values[alpha_id[id %in% .x]],
                              collapse = ",")
                     ))
  }

  # apply labels
  graph_obj$nodes_df <-
   ndf %>%
   mutate(label = case_when(

       .$fixed & is.null(custom_values)
       ~ paste0(ifelse(.$alpha_id %in% names(lab),
                        paste0(.$label, "@^{<i>", lab[.$alpha_id], "</i>}"),
                        .$alpha_id),
                " <font point-size=\"",
                sep_point_size, "\">",
                sep_opts()[fixed_sep],
                "</font> <i>",
                tolower(.$label), "</i> @_{ }"),

       .$fixed & !is.null(custom_values)
       ~ paste0(ifelse(.$alpha_id %in% names(lab),
                        paste0(.$label, "@^{<i>", lab[.$alpha_id], "</i>}"),
                        .$label),
                " <font point-size=\"",
                sep_point_size, "\">",
                sep_opts()[fixed_sep],
                "</font> <i>",
                tolower(.$label), "=", custom_values[.$alpha_id], "</i>@_{ }"),

       .$alpha_id %in% names(lab)
       ~ paste0(.$alpha_id, "@^{<i>", lab[.$alpha_id], "</i>}"),

       TRUE ~ .$alpha_id
     ))
  graph_obj
}
