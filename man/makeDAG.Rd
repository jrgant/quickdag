% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeDAG.R
\name{makeDAG}
\alias{makeDAG}
\title{Output, view, or embed DAGs.}
\usage{
makeDAG(
  graphcode = NULL,
  dagname = NULL,
  filetype = "pdf",
  text.nodes = NULL,
  box.nodes = NULL,
  solid.edges = NULL,
  dashed.edges = NULL,
  footnotes = NULL,
  direction = "LR",
  embed = FALSE,
  ...
)
}
\arguments{
\item{graphcode}{Partial graphViz code object, which will give you the most control over the appearance of your DAG.}

\item{dagname}{A path with which to name your DAG file, if \code{filetype} set to output format (see below).}

\item{filetype}{Output file format. Select from \code{pdf}, \code{png}, \code{svg}, or \code{view}. Setting the option to \code{view} will not save a file but will generate the diagram in your viewer. Defaults to \code{pdf}.}

\item{text.nodes}{A string containing the plain text nodes, separated by spaces.}

\item{box.nodes}{A string containing the boxed nodes, separated by spaces.}

\item{solid.edges}{A string specifying the paths you want to draw between measured covariates. Example: \code{"Alcohol -> Smoking Smoking -> Cancer"}.}

\item{dashed.edges}{A string specifying paths containing unmeasured covariates.}

\item{footnotes}{Add a footnote to the bottom of the graph.}

\item{direction}{Specify the direction of diagram flow. Defaults to \code{"LR"}, for left-right.}

\item{embed}{For use within R chunks in RMarkdown only. You will probably want to have the \code{echo} chunk option set to \code{FALSE}, unless you want to display the R code itself. The \code{embed} defaults to \code{TRUE}.}

\item{...}{Pass arguments to interior functions for PNG or SVG files. For instance, specify \code{height} and \code{width} arguments.}
}
\description{
Save directed acyclic graphs (DAGs) generated in DiagrammeR and export them to pdf, png, or svg format. Users may optionally choose to view the DAG without saving or to embed it in an RMarkdown document.
}
\note{
This is not a true DAG package in the sense that it will not prevent the inclusion of feedback loops or bidirectional arrows. It's meant mostly to create reasonable-looking DAGs quickly and easily with a minimum of layout or formatting code. DiagrammeR in general does a pretty good job at layout. Those interested in DAGs might check out other R packages like dagR or dagitty, both of which I've yet to explore in detail.

Suggestions and issue reports welcome at \url{https://github.com/jrgant/quickDAG/issues}!
}
\examples{
# Using your own graph code

# Examples have been removed, as makeDAG() is now deprecated.

}
\references{
A fair amount of the heavy lifting here is done thanks to code snippets from users HJAllen and puterleat on the following thread: \url{https://github.com/rich-iannone/DiagrammeR/issues/133}

Packages used: DiagrammeR, DiagrammeRsvg, rsvg
}
