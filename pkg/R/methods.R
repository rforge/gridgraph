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
    # set size to 0x0 inches to let graphviz set size of boundBox
    attrs$graph$size <- "0,0"
    # set edge weights to those specified in graph
    weights <- unlist(edgeWeights(graph))
    names(weights) <- edgeNames(graph)
    edgeAttrs$weight <- weights
    # let graphviz set node sizes unless specififed
    if (is.null(attrs$fixedsize)) {
        attrs$node$fixedsize <- "FALSE"
    }
    if (is.null(attrs$width)) {
        attrs$node$width <- ""
    }
    if (is.null(attrs$height)) {
        attrs$node$height <- ""
    }
    # create temporary Ragraph to extract boundBox info
    temp <- agopen(graph=graph, name=name, nodes=nodes, edges=edges, kind=kind, 
                   layout=TRUE, layoutType=layoutType, attrs=attrs, 
                   nodeAttrs=nodeAttrs, edgeAttrs=edgeAttrs, subGList=subGList,
                   edgeMode=edgeMode, recipEdges=recipEdges)
    # set graph size attribute with size generated in 'temp'
    attrs$graph$size <- graphSize(temp)
    
    agopen(graph=graph, name=name, nodes=nodes, edges=edges, kind=kind, 
           layout=TRUE, layoutType=layoutType, attrs=attrs, 
           nodeAttrs=nodeAttrs, edgeAttrs=edgeAttrs, subGList=subGList,
           edgeMode=edgeMode, recipEdges=recipEdges)
}
