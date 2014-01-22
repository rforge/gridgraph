\name{agopenSized}
\alias{agopenSized}
\title{
A function to obtain a \code{Ragraph} object with size set by graphviz
}
\description{
  This function will read in a \code{graph} object and create a
  \code{Ragraph} object using the \pkg{Rgraphviz} function \code{agopen}, but
  with the size determined by graphviz, and not the \code{device} size.
}
\usage{
agopenSized(graph, name, nodes, edges, kind = NULL, layoutType = "dot", 
            attrs = list(), nodeAttrs = list(), edgeAttrs = list(),
            subGList = list())
}
\arguments{
  All arguments the same as in \code{agopen}

  NB: argument \code{layout} is omitted from this function.
}
\details{
  While \code{agopen} can produce \code{Ragraph} objects laid out by
  graphviz, it currently (2013-01-22) produces graphs of the same size
  as the current \code{device}, or at a default size of 7x7 inches.

  This function performs exactly the same as \code{agopen} except it
  polls graphviz for the appropriate size of the graph and produces a
  laidout \code{Ragraph} object at this size.
}
\value{
  An object of class 'Ragraph'
}
\references{
  graphviz
%% ~put references to the literature/web site here ~
}
\author{
Ashley Noel Hinton
}
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
rag <- agopen(gnel, "")
grid.graph(rag)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
