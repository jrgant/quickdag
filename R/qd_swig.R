#' Generate a single-world intervention graph (SWIG)
#'
#' @description
#' Take a DAG graph object and, in the simplest case, create a single-world intervention template corresponding
#' to a world in which the fixed nodes are set to a given value. Alternatively, tell qd_swig which values fixed nodes
#' will be set to.
#'
#' @param graph.obj A DAG object created by \code{qd_dag()}.
#' @param fixed.nodes A vector containing the nodes to be intervened upon.
#' @param custom.values A named vector containing alternative labels identifying explicit values for fixed nodes (e.g., a = 1).
#' @param fixed.sep A character string indicating which character to use as a separator in fixed nodes. Defaults to "vlin". Run \code{sep_opts(T)} for available options.
#' @param sep.point.size A numerical value specifying the point size for fixed node separators.
#'
#' @examples
#' # Provide a DAG object and a list of nodes to be fixed
#' edges <- c("A -> Y",
#'            "L -> { A Y }")
#'
#' dag  <- qd_dag(edges)
#'
#' swig <- dag %>%
#'         qd_swig(fixed.nodes = "A",
#'                 custom.values = c("A" = "1"))
#'
#' swig %>% DiagrammeR::render_graph()
#'
#' @export qd_swig
#' @import DiagrammeR
#' @import purrr
#' @importFrom dplyr bind_rows mutate case_when if_else

qd_swig <- function(graph.obj,
                    fixed.nodes,
                    custom.values = NULL,
                    fixed.sep = "vlin",
                    sep.point.size = 15) {
  # graph.obj = graph
  # fixed = alpha IDs for fixed nodes
  ndf <- get_node_df(graph.obj)
  ndf$fixed <- with(ndf, ifelse(alpha.id %in% fixed.nodes, TRUE, FALSE))

  fx.pathlist <-
    map(
      .x = set_names(ndf$alpha.id, ndf$alpha.id),
      .f = function(x) {
        curr.id  <- with(ndf, id[alpha.id == x])
        # each path will include current node id by default
        # map() set up to drop the destination node
        ancestors <-
          get_paths(graph.obj, to = curr.id) %>%
          map(~ .x[.x != curr.id])


        fx.nodes <-
          ancestors %>%
          map(function(x) {
            detect(x, function(y) y %in% with(ndf, id[fixed]), .dir = "backward")
          })
        unique(unlist(fx.nodes))
      })

  fx.ancestors <- discard(fx.pathlist, function(x) is.null(x))


  # create labels for those in custom values
  if (is.null(custom.values)) {
    lab <-
      fx.ancestors %>%
      map_chr(~ with(ndf, paste0(tolower(label[id %in% .x]), collapse = ",")))
  } else {
    lab <-
      fx.ancestors %>%
      map_chr(~ with(ndf, paste0(tolower(label[id %in% .x]), "=",
                              custom.values[alpha.id[id %in% .x]],
                              collapse = ",")
                     ))
  }

  # apply labels
  graph.obj$nodes_df <-
   ndf %>%
   mutate(label = case_when(

       .$fixed & is.null(custom.values)
       ~ paste0(if_else(.$alpha.id %in% names(lab),
                        paste0(.$label, "@^{<i>", lab[.$alpha.id], "</i>}"),
                        .$alpha.id),
                " <font point-size=\"", sep.point.size, "\">", sep_opts()[fixed.sep], "</font> <i>",
                tolower(.$label), "</i> @_{ }"),

       .$fixed & !is.null(custom.values)
       ~ paste0(if_else(.$alpha.id %in% names(lab),
                        paste0(.$label, "@^{<i>", lab[.$alpha.id], "</i>}"),
                        .$label),
                " <font point-size=\"", sep.point.size, "\">", sep_opts()[fixed.sep], "</font> <i>",
                tolower(.$label), "=", custom.values[.$alpha.id], "</i>@_{ }"),

       .$alpha.id %in% names(lab)
       ~ paste0(.$alpha.id, "@^{<i>", lab[.$alpha.id], "</i>}"),

       TRUE ~ .$alpha.id
     ))
  graph.obj
}
