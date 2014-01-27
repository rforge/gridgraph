
# Draw an Ragraph laid out graph using grid

# Code for "node" grobs

node <- function(label, x=.5, y=.5,
                 shape="plain",
                 height=NULL, lwidth=NULL, rwidth=NULL,
                 name=label,
                 color="black",
                 fillcolor="transparent",
                 fontcolor="black",
                 fontsize=10) {
    # Shrink text if necessary
    cex <- 1
    if (!(is.null(height) || is.null(lwidth) || is.null(rwidth))) {
        tw <- convertWidth(grobWidth(textGrob(label,
                                              gp=gpar(fontsize=fontsize)))*1.4,
                           "inches", valueOnly=TRUE)
        nw <- convertWidth(lwidth + rwidth, "inches",
                           valueOnly=TRUE)
        if (tw > nw)
            cex <- nw/tw
        th <- convertHeight(grobHeight(textGrob(label,
                                                gp=gpar(fontsize=fontsize)))*1.4,
                            "inches", valueOnly=TRUE)
        nh <- convertHeight(height, "inches", valueOnly=TRUE)
        if (th > nh && nh/th < cex)
            cex <- nh/th
    }
    lab <- makeLabel(label, x, y, fontcolor, fontsize, cex)
    if (is.null(height)) {
        height <- grobHeight(lab)
    }
    if (is.null(lwidth) || is.null(rwidth)) {
        lwidth <- 0.5*grobWidth(lab)
        rwidth <- 0.5*grobWidth(lab)
    }
    a <- 0.5 * (lwidth + rwidth)
    b <- 0.5 * height
    if (shape == "circle") {
        box <- circleGrob(x, y, r=a, name="box",
                          gp=gpar(col=color, fill=fillcolor))
    } else if (shape == "ellipse") { 
      angle <- seq(0, 2*pi, length=101)
      box <- polygonGrob(x + a*cos(angle),
                         y + b*sin(angle),
                         name="box",
                         gp=gpar(col=color, fill=fillcolor))
    } else if (shape == "polygon" || shape == "triangle" ||
               shape == "pentagon" || shape == "hexagon" ||
               shape == "septagon" || shape == "octagon"
               ) {
        vertices <- switch(shape,
                           polygon = 4,
                           triangle = 3,
                           pentagon = 5,
                           hexagon = 6,
                           septagon = 7,
                           octagon = 8)
        angle <- seq(0, 2*pi, length=vertices + 1)[-(vertices + 1)]
        if (vertices %% 2 != 0) {
          angle <- angle + pi/2
        } else {
          angle <- angle + pi/vertices + pi/2
          # expand polygon vertically to fill box
          b <- b*(1/cos(pi/vertices))
        }
        if (vertices %% 4 == 0)
          # expand polygon horizontally to fill box
          a <- a*(1/cos(pi/vertices))
        
        box <- polygonGrob(x + a*cos(angle), y + b*sin(angle),
                           name="box",
                           gp=gpar(col=color, fill=fillcolor))
    } else if (shape == "box") {
        box <- rectGrob(x, y,
                        width=lwidth + rwidth,
                        height=height,
                        name="box",         
                        gp=gpar(col=color, fill=fillcolor))
    } else { # plain
        warning("Unsupported node shape; using 'box'")
        box <- rectGrob(x, y,
                        width=lwidth + rwidth,
                        height=height,
                        name="box", gp=gpar(col=NA, fill=NA))
    }
    gTree(children=gList(box, lab),
          name=gsub("\n", "", name), cl="node")
}

# Dimensions of node come from "box" child (which depends on the shape)
grobX.node <- function(x, theta) {
    grobX(getGrob(x, "box"), theta)
}

grobY.node <- function(x, theta) {
    grobY(getGrob(x, "box"), theta)
}

drawNode <- function(node) {
    name <- name(node)
    shape <- shape(node)
    if (is.na(shape)) {
        shape <- "plain"
    }
    height <- getNodeHeight(node)
    lwidth <- getNodeLW(node)
    rwidth <- getNodeRW(node)
    col <- color(node)
    if (is.na(col) || col == "") {
        col <- "black"
    }
    fill <- fillcolor(node)
    if (is.na(fill) || fill == "") {
        fill <- "transparent"
    }
    fontcol <- labelColor(txtLabel(node))
    if (is.na(fontcol)) {
        fontcol <- "black"
    }
    fontsize <- labelFontsize(txtLabel(node))
    label <- labelText(txtLabel(node))
    xy <- getNodeXY(node)
    grid.draw(node(label,
                   unit(xy$x, "native"),
                   unit(xy$y, "native"),
                   shape=shape,
                   height=unit(height, "native"),
                   lwidth=unit(lwidth, "native"),
                   rwidth=unit(rwidth, "native"),
                   color=col, fillcolor=fill,
                   fontcolor=fontcol, fontsize=fontsize))
}

makeLabel <- function(label, x, y, col, fontsize, cex) {
    textGrob(label, x, y,
             gp=gpar(col=col, fontsize=fontsize, cex=cex),
             name="label")
}

makeCurve <- function(curve, col, lwd, lty) {
    controlPoints <- pointList(curve)
    bezierGrob(unit(sapply(controlPoints, "[" ,1), "native"),
               unit(sapply(controlPoints, "[" ,2), "native"),
               gp=gpar(col=col, lwd=lwd, lty=lty))
}

drawCurve <- function(curve, col, lwd, lty) {
    grid.draw(curveGrob(curve, col, lwd, lty))
}

makeArrow <- function(arrowType, arrowsize, startX, startY, endX, endY, 
                      col, lwd, lty) {
  if (arrowType == "normal") arrowType <- "closed"
  if (arrowType == "vee") arrowType <- "open"
  if (arrowType == "none" || arrowType == "open" || arrowType == "closed") {
    arrow <- NULL
    if (arrowType != "none") {
      # FIXME: using a calculated 'length' does not scale when adjusted by
      # arrowsize, so graphviz default length 10 used
      arrowlen <- unit(arrowsize*10, "native")
      arrow <- arrow(angle=20, type=arrowType, length=arrowlen)
    }
    segmentsGrob(startX, startY,
                 endX, endY,
                 default.units="native",
                 arrow=arrow,
                 gp=gpar(col=col, fill=col,
                         lwd=lwd, lty=lty))
  } else if (arrowType == "dot" || arrowType == "odot") {
    # FIXME: does not scale correctly compared to graphviz. documentation shows
    # graphviz sets default radius of 2, but does not work here. using 'length'
    # gets reasonably close results for arrowsize ~= 1
    dx <- endX - startX
    dy <- endY - startY
    length <- sqrt(dx^2 + dy^2) 
    theta <- atan2(dy, dx)
    r <- length/2 * arrowsize
    # FIXME: transparent circle will show edge inside if arrowsize > 1 due to
    # Rgraphviz not passing through arrow info at layout
    if (arrowType == "odot") fill <- "transparent" else fill <- col
    head <- circleGrob(endX - r*cos(theta), endY - r*sin(theta),
                       r=r, default.units="native", name="dothead",
                       gp=gpar(col=col, fill=fill, lwd=lwd, lty=lty))
    segment <- NULL
    # as different arrowheads cannot currently be passed through Rgraphviz's
    # agopen when laying out a line segment connects the edge to the dot head
    # when the arrowsize is less than the gap 'length'
    if (arrowsize < 1) {
      segment <- segmentsGrob(startX, startY,
                              grobX("dothead", theta*180/pi),
                              grobY("dothead", theta*180/pi),
                              default.units="native",
                              gp=gpar(col=col, fill=col,
                                      lwd=lwd, lty=lty))
    } 
    gList(head, segment)
  } else if (arrowType == "box" || arrowType == "obox") {
    #FIXME: calculation of length, r, etc has same problems as with "dot" above
    dx <- endX - startX
    dy <- endY - startY
    length <- sqrt(dx^2 + dy^2) 
    theta <- atan2(dy, dx)
    r <- length/2 * arrowsize
    boxvp <- viewport(x=endX - r*cos(theta), y=endY - r*sin(theta),
                      width=r*2, height=r*2, default.units="native",
                      angle=theta*180/pi)
    if (arrowType == "obox") fill <- "transparent" else fill <- col
    head <- rectGrob(vp=boxvp, gp=gpar(col=col, fill=fill,
                                       lwd=lwd, lty=lty))
    segment <- NULL
    if (arrowsize < 1) {
      segment <- segmentsGrob(startX, startY,
                              grobX("dothead", theta*180/pi),
                              grobY("dothead", theta*180/pi),
                              default.units="native",
                              gp=gpar(col=col, fill=col,
                                      lwd=lwd, lty=lty))
    } 
    gList(head, segment)
  }
}

makeEdge <- function(edge, edgemode) {
    if (!length(edge@lwd))
        edge@lwd <- 1    
    if (!length(edge@lty))
        edge@lty <- "solid"
    splines <- splines(edge)
    n <- length(splines)
    col <- color(edge)
    curves <- lapply(splines, makeCurve, col=col, lwd=edge@lwd, lty=edge@lty)
    
    # Edge label
    if (length(labelText(txtLabel(edge))) != 0) {
        fontcol <- labelColor(txtLabel(edge))
        if (length(fontcol) == 0) {
            fontcol <- "black"
        }
        fontsize <- labelFontsize(txtLabel(edge))
        label <- labelText(txtLabel(edge))
        xy <- labelLoc(txtLabel(edge))
        x <- unit(getX(xy), "native")
        y <- unit(getY(xy), "native")
        cex <- 1
        lab <- list(makeLabel(label, x, y, fontcol, fontsize, cex))
    } else {
        lab <- list()
    }
        
    firstCP <- pointList(splines[[n]])[[1]]
    lastCP <- pointList(splines[[n]])[[4]]
    arrowsize <- as.numeric(arrowsize(edge))
    arrowhead <- arrowhead(edge)
    arrowtail <- arrowtail(edge)

    # "back" arrow    
    if (edgemode == "undirected" || edge@dir == "forward" ||
        (is.na(getX(sp(edge))) || is.na(getY(sp(edge)))) ||
        (getX(sp(edge)) == 0 && getY(sp(edge)) == 0)) {
      start <- list()
    } else {
      start <- list(makeArrow(arrowtail, arrowsize,
                              firstCP[1], firstCP[2], 
                              getX(sp(edge)), getY(sp(edge)), 
                              col, edge@lwd, edge@lty))
    }
    
    # "forward" arrow
    if (edgemode == "undirected" || edge@dir == "back" ||
        (is.na(getX(ep(edge))) || is.na(getY(ep(edge)))) ||
        (getX(ep(edge)) == 0 && getY(ep(edge)) == 0)) {
      end <- list()
    } else {
      end <- list(makeArrow(arrowhead, arrowsize,
                            lastCP[1], lastCP[2], 
                            getX(ep(edge)), getY(ep(edge)), 
                            col, edge@lwd, edge@lty))
    }
    
    gTree(children=do.call("gList", c(curves, start, end, lab)))
}

drawEdge <- function(edge, edgemode) {
    grid.draw(makeEdge(edge, edgemode))
}

grid.graph <- function(rag, newpage=FALSE, nodesOnTop=TRUE) {
    if (!is(rag, "Ragraph") || !laidout(rag))
        stop("Must have a laid out Ragraph object")
    if (newpage) {
        grid.newpage()
    }
    # (x, y) locations of all the nodes
    # The order is the same as the nodes in
    # the original graphNEL
    bb <- boundBox(rag)
    # Ensure aspect ratio
    pushViewport(viewport(width=unit(getX(upRight(bb))/72, "inches"),
                          height=unit(getY(upRight(bb))/72, "inches"),
                          layout=grid.layout(1, 1,
                              widths=(getX(upRight(bb)) - getX(botLeft(bb))) /
                                     (getY(upRight(bb)) - getY(botLeft(bb))),
                              respect=TRUE)))
    pushViewport(viewport(layout.pos.col=1,
                          xscale=c(getX(botLeft(bb)), getX(upRight(bb))),
                          yscale=c(getY(botLeft(bb)), getY(upRight(bb)))))
    if (nodesOnTop) {
        lapply(AgEdge(rag), drawEdge, edgemode(rag))
        lapply(AgNode(rag), drawNode)
    } else {
        lapply(AgNode(rag), drawNode)
        lapply(AgEdge(rag), drawEdge, edgemode(rag))
    }
    upViewport(2)
}
