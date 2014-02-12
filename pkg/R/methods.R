## Methods for laying out and plotting Ragraph objects

## the following methods return and Ragraph's dimensions in inches
## used for determining size of graph used to override Rgraphciz's defaults, and
## for determining appropriate sizes for output to device
graphWidth <- function (graph) {
  return(getX(upRight(boundBox(graph)))/72)
}
graphHeight <- function (graph) {
    return(getY(upRight(boundBox(graph)))/72)
}

## return an Ragraph's size as character string of form "x,y" for setting size
## attribute for agopen()
graphSize <- function (graph) {
    return(paste(graphWidth(graph), graphHeight(graph), sep=","))
}

groomAttrs <- function(attrs) {
    # checks to see if user has explicitly set a graph size in attrs list
    # if nothing has been specified set to "0,0" to allow 
    if (is.null(attrs$graph$size)) attrs$graph$size <- ""
    
    # ANOTHER EXPLANATION
    if (is.null(attrs$node$fixedsize)) {
        attrs$node$fixedsize <- "FALSE"
    }
    if (is.null(attrs$node$width)) {
        attrs$node$width <- ""
    }
    if (is.null(attrs$node$height)) {
        attrs$node$height <- ""
    }
    attrs
}

groomEdgeAttrs <- function(graph, edgeAttrs) {
    # EXPLAIN ME
    weights <- unlist(edgeWeights(graph))
    names(weights) <- edgeNames(graph)
    edgeAttrs$weight <- weights
    edgeAttrs
}

## 2014-01-31: As of Rgraphviz v2.2.1
## + agopen() sets the size of an Ragraph object to either the size of the
##   open graphics device, or 7x7 inches
## + agopen() does not pass pass through edge weight information
## agopenTrue():
## + produces an Ragraph object of the size determined by graphviz
## + passes through accurate edge weight information
## NB: agopenTrue() does not accept argument 'layout' as it should always
## lay out the graph
agopenTrue <- function(graph, name, nodes, edges, kind = NULL,
                        layoutType = "dot", attrs = list(), nodeAttrs = list(), 
                        edgeAttrs = list(), subGList = list(),
                        edgeMode = edgemode(graph),
                        recipEdges = c("combined", "distinct")) {
    # FIXME: explain please
    attrs <- groomAttrs(attrs)
    # set edge weights to those specified in graph
    edgeAttrs <- groomEdgeAttrs(graph, edgeAttrs)

    agopen(graph=graph, name=name, nodes=nodes, edges=edges, kind=kind, 
           layout=TRUE, layoutType=layoutType, attrs=attrs, 
           nodeAttrs=nodeAttrs, edgeAttrs=edgeAttrs, subGList=subGList,
           edgeMode=edgeMode, recipEdges=recipEdges)
}
