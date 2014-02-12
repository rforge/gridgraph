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

## groomAttrs() takes the 'attrs' list passed to agopenTrue() and alters
## certain values to preferred defaults, overriding Rgraphviz defaults.
## Arguments:
##   attrs - list
## Values
##   attrs - list
groomAttrs <- function(attrs) {
    # assume that is user has a desired graph size this will be passed to
    # agopenTrue() in the attrs list by:
    #   agopenTrue(..., attrs=list(graph=list(size="XX,YY")))
    # if attrs$graph$size in NULL assume user has not set a size
    # and set this "" to allow graphviz to
    # determine size of graph
    if (is.null(attrs$graph$size)) attrs$graph$size <- ""
    
    # assume if user wants to control node sized this will be passed to
    # agopenTrue() in the attrs list by:
    #   agopenTrue(..., attrs=list(node=list(fixedsize="VALUE", width="XX",
    #                                        height="YY")))
    # if attrs$node$fixedsize attrs$node$width attrs$node$height are NULL
    # assume user has not set a size and set to "" to allow graphviz to
    # determine values
    if (is.null(attrs$node$fixedsize)) {
        attrs$node$fixedsize <- ""
    }
    if (is.null(attrs$node$width)) {
        attrs$node$width <- ""
    }
    if (is.null(attrs$node$height)) {
        attrs$node$height <- ""
    }

    # return modified attrs list
    attrs
}

## groomEdgeAttrs() takes the edgeAttrs list passed to agopenTrue() and
## certain values to preferred defaults, overriding Rgraphviz defaults.
## Arguments:
##   graph - "graphNEL" object
##   edgeAttrs - list
## Values
##   edgeAttrs - list
groomEdgeAttrs <- function(graph, edgeAttrs) {
    # Rgraphviz does not pass edge weights from a graph through to graphviz
    # extract edge weights from graph and add named vector of weights to
    # edgeAttrs list
    weights <- unlist(edgeWeights(graph))
    names(weights) <- edgeNames(graph)
    edgeAttrs$weight <- weights

    # return modified edgeAttrs list
    edgeAttrs
}

## agopenTrue() is a wrapper for Rgraphviz's agopen()
## it replaces certain Rgraphviz defaults with preferred defaults
## and attempts to pass through graph features which Rgraphviz does not
## NB: agopenTrue() does not accept a 'laidout' argument as graph should
##     always be laid out
agopenTrue <- function(graph, name, nodes, edges, kind = NULL,
                        layoutType = "dot", attrs = list(), nodeAttrs = list(), 
                        edgeAttrs = list(), subGList = list(),
                        edgeMode = edgemode(graph),
                        recipEdges = c("combined", "distinct")) {
    # modifies attrs list to replace Rgraphviz defaults with preferred default
    # values
    attrs <- groomAttrs(attrs)
    # set edge weights to those specified in graph as Rgraphviz 
    edgeAttrs <- groomEdgeAttrs(graph, edgeAttrs)

    agopen(graph=graph, name=name, nodes=nodes, edges=edges, kind=kind, 
           layout=TRUE, layoutType=layoutType, attrs=attrs, 
           nodeAttrs=nodeAttrs, edgeAttrs=edgeAttrs, subGList=subGList,
           edgeMode=edgeMode, recipEdges=recipEdges)
}
