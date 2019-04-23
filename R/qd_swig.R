#' Generate a single-world intervention graph (SWIG)
#'
#' @description
#' Take a DAG graph object and, in the simplest case, create a single-world intervention template corresponding
#' to a world in which the fixed nodes are set to a given value. Alternatively, tell qd_swig which values fixed nodes
#' will be set to.
#'
#' @param graph.obj A DAG object created by \code{qd_dag()}.
#' @param fixed.nodes A vector containing the nodes to be intervened upon.
#' @param custom.labels A named vector containing alternative labels identifying explicit values for fixed nodes (e.g., a = 1).
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
#'                 custom.labels = c("A" = "1"))
#'
#' swig %>% render_graph()
#'
#' @export qd_swig
#' @import DiagrammeR
#' @import purrr
#' @importFrom dplyr bind_rows mutate case_when if_else

qd_swig <- function(graph.obj,
                    fixed.nodes,
                    custom.labels = NULL,
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

  if (is.null(custom.labels)) {
    lab <-
      fx.ancestors %>%
      map_chr(~ with(ndf, paste0(tolower(alpha.id[id %in% .x]), collapse = ",")))
  } else {
    lab <-
      fx.ancestors %>%
      map_chr(~ with(ndf, paste0(tolower(alpha.id[id %in% .x]), "=",
                              custom.labels[alpha.id[id %in% .x]],
                              collapse = ",")
                     ))
  }


  # apply labels
  graph.obj$nodes_df <-
    ndf %>%
    mutate(label = case_when(

      .$alpha.id %in% names(lab) & !fixed
      ~ paste0(.$alpha.id, "@^{<i>", lab[.$alpha.id], "</i>}"),

      .$fixed & is.null(custom.labels)
      ~ paste0(.$alpha.id, " <font point-size=\"", sep.point.size, "\">", sep_opts()[fixed.sep], "</font> <i>",
               tolower(.$alpha.id), "</i> @_{ }"),

      .$fixed & !is.null(custom.labels)
      ~ paste0(.$alpha.id, " <font point-size=\"", sep.point.size, "\">", sep_opts()[fixed.sep], "</font> <i>",
               tolower(.$alpha.id), "=", custom.labels[.$alpha.id], "</i>@_{ }"),

      TRUE ~ .$alpha.id
    ))
  graph.obj
}
