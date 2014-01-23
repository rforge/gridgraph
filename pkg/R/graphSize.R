## Create an Ragraph object with size determined by graphviz

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

## Rgraphviz's agopen() sets the size of an Ragraph object to either the size
## of the open graphics device, or 7x7 inches.
## agopenSized produces an Ragraph object of the size determined by graphviz
## NB: agopenSized() does not accept argument 'layout' as it should always
## lay out the graph
agopenSized <- function(graph, name, nodes, edges, kind = NULL,
                        layoutType = "dot", attrs = list(), nodeAttrs = list(), 
                        edgeAttrs = list(), subGList = list(),
                        edgeMode = edgemode(graph),
                        recipEdges = c("combined", "distinct")) {
    # set size to 0x0 inches to let graphviz set size of boundBox
    attrs$graph$size <- "0,0"
    # set edge weights to those specified in graph (Rgraphviz agopen() does not
    # do this
    weights <- unlist(edgeWeights(graph))
    names(weights) <- edgeNames(graph)
    edgeAttrs$weight <- weights
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
