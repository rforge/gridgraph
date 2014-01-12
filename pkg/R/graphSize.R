## Create an Ragraph object with size determined by graphviz

## Rgraphviz's agopen() sets the size of an Ragraph object to either the size
## of the open graphics device, or 7x7 inches.
## agopenSized produces an Ragraph object of the size determined by graphviz
agopenSized <- function(graph, name, nodes, edges, kind = NULL,
                        layoutType = "dot", attrs = list(), nodeAttrs = list(), 
                        edgeAttrs = list(), subGList = list()) {
    # set size to 0x0 inches to let graphviz set size of boundBox
    attrs$graph$size <- "0,0"
    # create temporary Ragraph to extract boundBox info
    temp <- agopen(graph=graph, name=name, nodes=nodes, edges=edges, kind=kind, 
                   layout=TRUE, layoutType=layoutType, attrs=attrs, 
                   nodeAttrs=nodeAttrs, edgeAttrs=edgeAttrs, subGList=subGList)
    # extract boundBox and use to calculate size in inches               
    xBound <- getX(upRight(boundBox(temp)))/72
    yBound <- getY(upRight(boundBox(temp)))/72
    boundsInches <- paste(xBound, yBound, sep=",")
    
    attrs$graph$size <- boundsInches
    
    agopen(graph=graph, name=name, nodes=nodes, edges=edges, kind=kind, 
           layout=TRUE, layoutType=layoutType, attrs=attrs, 
           nodeAttrs=nodeAttrs, edgeAttrs=edgeAttrs, subGList=subGList)
}

