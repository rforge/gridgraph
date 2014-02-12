\name{agopenTrue}
\alias{agopenTrue}
\title{
  A function to obtain a laidout \code{Ragraph} object
}
\description{
  This function is a wrapper for the \pkg{Rgraphviz} function
  \code{agopen} and will produce a laidout \code{Ragraph} object with
  layout more true to the result produced by graphviz.
}
\usage{
agopenTrue(graph, name, nodes, edges, kind = NULL, layoutType = "dot", 
           attrs = list(), nodeAttrs = list(), edgeAttrs = list(),
           subGList = list(), edgeMode = edgemode(graph),
           recipEdges = c("combined", "distinct"))
}
\arguments{
  \item{graph}{An object of class \code{graphNEL}}
  \item{nodes}{A list of \code{pNode} objects}
  \item{edges}{A list of \code{pEdge} objects}
  \item{name}{The name of the graph}
  \item{kind}{The type of graph}
  \item{layoutType}{Defines the layout engine.  Defaults to dot, and see
    \code{graphvizCapabilities()$layoutTypes} for possible values.}
  \item{attrs}{A list of graphviz attributes}
  \item{nodeAttrs}{A list of specific node attributes}
  \item{edgeAttrs}{A list of specific edge attributes}
  \item{subGList}{A list describing subgraphs for the graph parameter}
  \item{edgeMode}{Whether the graph is directed or undirected}
  \item{recipEdges}{How to handle reciprocated edges, defaults to
    \code{combined}}
}
\details{
  As of \code{Rgraphviz} version 2.2.1 (2013-01-31) \code{agopen}:
  \itemize{
    \item Produces graphs of the same size as the current device,
    or at a default size of 7x7 inches.
    \item Forces nodes to fixed default height and width.
    \item Does not pass through edge weight information.
  }

  This function returns an \code{Ragraph} object with graph
  and node sizes set by graphviz or by the user.  It also ensures
  edge weight information is passed through.
}
\value{
  An object of class \code{Ragraph}
}
\references{
  graphviz
%% ~put references to the literature/web site here ~
}
\author{Ashley Noel Hinton}
\seealso{
  \code{\link{agopen}}
}
\examples{
gnel <- new("graphNEL",
            nodes=letters[1:3],
            edgeL=list(a=list(edges=c("b", "c")),
                       b=list(),
                       c=list()),
            edgemode="directed")
rag <- agopenTrue(gnel, "")
grid.graph(rag)
}
\keyword{ dplot }
