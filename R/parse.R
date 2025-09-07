#' @title Parse nodes
#' @param edgestring A string specifying node-edge relationships
#' @export
parse_nodes <- function(edgestring) {
  nodes <- stringr::str_extract_all(edgestring, stringr::boundary("word"))
  nodes
}


#' @title Parse edges based on alpha IDs
#' @inheritParams parse_nodes
#' @export
parse_edges <- function(edgestring) {
  edge <- data.frame(edge = unlist(stringr::str_extract_all(edgestring, EDGEPAT)))
  edge_locs <- as.data.frame(stringr::str_locate_all(edgestring, EDGEPAT))
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
