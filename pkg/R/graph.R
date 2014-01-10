
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
    } else if (shape == "triangle" | shape == "pentagon" | shape == "hexagon" | 
               shape == "septagon" | shape == "octagon") {
        if (shape == "triangle") vertices = 3
        else if (shape == "pentagon") vertices = 5
        else if (shape == "hexagon") vertices = 6
        else if (shape == "septagon") vertices = 7
        else if (shape == "octagon") vertices = 8
        angle <- seq(0, 2*pi, length=vertices + 1)[-(vertices + 1)]
        if (vertices %% 2 != 0) angle <- angle + pi/2
        else angle <- angle + pi/vertices + pi/2
        box <- polygonGrob(x + a*cos(angle),
                           y + b*sin(angle),
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

manualArrow <- function(x1, y1, x2, y2,
                        col, lwd, lty, arrowhead, arrowsize) {
    x1i <- convertX(unit(x1, "native"), "inches", valueOnly=TRUE)
    x2i <- convertX(unit(x2, "native"), "inches", valueOnly=TRUE)
    y1i <- convertY(unit(y1, "native"), "inches", valueOnly=TRUE)
    y2i <- convertY(unit(y2, "native"), "inches", valueOnly=TRUE)
    dx <- x2i - x1i
    dy <- y2i - y1i
    theta <- atan2(dy, dx)
    len <- convertWidth(arrowsize, "inches", valueOnly=TRUE)
    if (arrowhead == "dot") {
        circleGrob(x2i - len/2*cos(theta), y2i - len/2*sin(theta),
                   r=len/2, default.units="inches",
                   gp=gpar(col=col, fill=col, lwd=lwd, lty=lty))
    } else {
        NULL
    }
}

makeArrow <- function(arrowType, arrowsize, startX, startY, endX, endY, 
                      col, lwd, lty) {
    if (arrowType == "none") {
        arrow <- NULL
    } else {
        arrow <- arrow(angle=15, type=arrowType, length=arrowsize)
    }
    z <- segmentsGrob(startX, startY,
                      endX, endY,
                      default.units="native",
                      arrow=arrow,
                      gp=gpar(col=col, fill=col,
                              lwd=lwd, lty=lty))
}

makeEdge <- function(edge, arrowlen, edgemode) {
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
        
    # FIXME:  assumes arrow at end of edge
    firstCP <- pointList(splines[[n]])[[1]]
    lastCP <- pointList(splines[[n]])[[4]]
    arrowsize <- as.numeric(arrowsize(edge))
    if (length(arrowsize) && is.finite(arrowsize)) {
        # arrowsize <- unit(10*arrowsize*edge@lwd*get.gpar()$lex, "points")
        arrowsize <- unit(10*arrowsize, "points")
    } else {
        # Stupid default set by grid.graph()
        arrowsize <- arrowlen
    }
    arrowhead <- arrowhead(edge)
    arrowtail <- arrowtail(edge)

    # "back" arrow    
    if (edgemode == "directed" && 
        (edge@dir == "both" || edge@dir == "back")) {
            start <- list(makeArrow(arrowtail, arrowsize,
                                    firstCP[1], firstCP[2], 
                                    getX(sp(edge)), getY(sp(edge)), 
                                    col, edge@lwd, edge@lty))
    } else {
        start <- list()
    }
    
    # "forward" arrow
    if (edgemode == "directed" && 
        (edge@dir == "both" || edge@dir == "forward")) {
            end <- list(makeArrow(arrowhead, arrowsize,
                                  lastCP[1], lastCP[2], 
                                  getX(ep(edge)), getY(ep(edge)), 
                                  col, edge@lwd, edge@lty))
    } else {
        end <- list()
    }
    
    gTree(children=do.call("gList", c(curves, start, end, lab)))
}

drawEdge <- function(edge, arrowlen, edgemode) {
    grid.draw(makeEdge(edge, arrowlen, edgemode))
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
    # FIXME: needs better calculation for arrow head size!
    # Set arrowlen based on graph size
    arrowlen <- min(.02*unit(getX(upRight(bb)) - getX(botLeft(bb)), "native"),
                    .02*unit(getY(upRight(bb)) - getY(botLeft(bb)), "native"))
    # Ensure aspect ratio
    pushViewport(viewport(layout=grid.layout(1, 1,
                            widths=(getX(upRight(bb)) - getX(botLeft(bb))) /
                                   (getY(upRight(bb)) - getY(botLeft(bb))),
                            respect=TRUE)))
    pushViewport(viewport(layout.pos.col=1,
                          xscale=c(getX(botLeft(bb)), getX(upRight(bb))),
                          yscale=c(getY(botLeft(bb)), getY(upRight(bb)))))
    if (nodesOnTop) {
        lapply(AgEdge(rag), drawEdge, arrowlen, edgemode(rag))
        lapply(AgNode(rag), drawNode)
    } else {
        lapply(AgNode(rag), drawNode)
        lapply(AgEdge(rag), drawEdge, arrowlen, edgemode(rag))
    }
    upViewport(2)
}
