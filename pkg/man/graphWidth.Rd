\name{graphWidth}
\alias{graphWidth}
\alias{graphHeight}
\title{
  Get width/height of an \code{Ragraph} object
}
\description{
  These functions will return the width or height of an \pkg{Rgraphviz}
  \code{Ragraph} object in inches.
}
\usage{
graphWidth(graph)
graphHeight(graph)
}
\arguments{
  \item{graph}{a laidout \code{Ragraph} object}
}
\value{
  A numeric vector of length 1.
}
\references{
  graphviz
%% ~put references to the literature/web site here ~
}
\author{
Ashley Noel Hinton
}
\seealso{
  \code{\link{Ragraph}}
}
\examples{
gnel <- new("graphNEL",
            nodes=letters[1:3],
            edgeL=list(a=list(edges=c("b", "c")),
                       b=list(),
                       c=list()),
            edgemode="directed")
rag <- agopenTrue(gnel, "")
graphWidth(rag)
graphHeight(rag)
}
\keyword{ dplot }
