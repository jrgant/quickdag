# This function will take an edgelist in the form that qd_dag takes and feed
#  it to dagitty in order to identify adjustment sets. Can specify additional
#  dagitty arguments to adjustmentSets() as well.
qd_todagitty <- function(edgelist, diagram_type = "dag", showplot = FALSE,
                         adjustmentvars, ...) {

  require(dagitty)

  dagitty.obj <- dagitty(paste(diagram_type, "{",
                               paste(edgelist, collapse = "; "),
                               "}"),
                         layout = TRUE)

  ## optional to show dagitty plot
  if (showplot) {
    plot(dagitty.obj)
  }

  ## use dagitty's algorithm to identify adjustment sets
  if (adjustmentvars) {
    adjustmentSets(dagitty.obj, ...)
  }

}
