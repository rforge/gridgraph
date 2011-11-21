\name{grid.graph}
\alias{grid.graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw a Node-and-Edge Graph
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  Take a description of the layout of a graph, as produced by
  the \pkg{Rgraphviz} package, and draw it.
}
\usage{
grid.graph(rag, newpage = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rag}{
    An Ragraph object (must be laid out).
%%     ~~Describe \code{rag} here~~
}
\item{newpage}{
  Logical value indicating whether to draw the graph on a new page.
%%     ~~Describe \code{newpage} here~~
}
}
\references{
  graphviz
%% ~put references to the literature/web site here ~
}
\author{
Paul Murrell
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

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
