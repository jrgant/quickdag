#' @title Parse nodes
#' @inheritParams qd_dag
#' @rdname node-parsing
#' @export
parse_nodes <- function(edgelist) {
  nodes <- unlist(lapply(edgelist,
                         stringr::str_extract_all,
                         pattern = stringr::boundary("word")))
  unique(nodes)
}


#' @title Parse edges in a graph specification
#' @param edgestring A single string containing some set of node-edge relationships
#' @rdname edge-parsing
#' @export
parse_edgestring <- function(edgestring) {
  edgepat <- "\\<\\-\\>|\\-\\>|\\<\\-"
  edge <- data.frame(edge = unlist(stringr::str_extract_all(edgestring, edgepat)))
  edge_locs <- as.data.frame(stringr::str_locate_all(edgestring, edgepat))
  edge_table <- cbind(edge, edge_locs)

  edge_table <- edge_table %>%
    dplyr::mutate(
      lstart = tidyr::replace_na(dplyr::lag(end) + 1, 1),
      lend = start - 1,
      rstart = end + 1,
      rend = tidyr::replace_na(dplyr::lead(start) - 1, stringr::str_length(edgestring))
    )

  edge_mini_dfs <- lapply(seq_len(nrow(edge_table)), \(.x) {
    curr <- edge_table[.x, ]
    switch(curr$edge,
           "->" = {
             parents <- parse_nodes(substring(edgestring, curr$lstart, curr$lend))
             children <- parse_nodes(substring(edgestring, curr$rstart, curr$rend))
             expand.grid(from_alpha = unlist(parents), to_alpha = unlist(children))
           },
           "<-" = {
             parents <- parse_nodes(substring(edgestring, curr$rstart, curr$rend))
             children <- parse_nodes(substring(edgestring, curr$lstart, curr$lend))
             expand.grid(from_alpha = unlist(parents), to_alpha = unlist(children))
           },
           "<->" = {
             parents <- parse_nodes(substring(edgestring, curr$lstart, curr$lend))
             children <- parse_nodes(substring(edgestring, curr$rstart, curr$rend))
             rbind(expand.grid(from_alpha = unlist(parents), to_alpha = unlist(children)),
                   expand.grid(from_alpha = unlist(children), to_alpha = unlist(parents)))
           })
  })

  Reduce(rbind, edge_mini_dfs)
}

#' @rdname edge-parsing
#' @inheritParams qd_dag
#' @export
parse_edges <- function(edgelist) {
  all_edges <- lapply(edgelist, parse_edgestring)
  Reduce(rbind, all_edges)
}
