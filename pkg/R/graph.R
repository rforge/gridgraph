
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
    lab <- textGrob(label, x, y,
                    gp=gpar(col=fontcolor, fontsize=fontsize, cex=cex),
                    name="label")
    if (is.null(height)) {
        height <- grobHeight(lab)
    }
    if (is.null(lwidth) || is.null(rwidth)) {
        lwidth <- 0.5*grobWidth(lab)
        rwidth <- 0.5*grobWidth(lab)
    }
    if (shape == "circle") {
        box <- circleGrob(x, y, r=lwidth, name="box",
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

makeEdge <- function(edge, arrowlen) {
    if (!length(edge@lwd))
        edge@lwd <- 1    
    if (!length(edge@lty))
        edge@lty <- "solid"
    splines <- splines(edge)
    n <- length(splines)
    col <- color(edge)
    curves <- lapply(splines, makeCurve, col=col, lwd=edge@lwd, lty=edge@lty)
    # FIXME:  assumes arrow at end of edge
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
    if (!length(arrowhead) || arrowhead == "" || arrowhead == "normal") {
        arrow <- arrow(angle=15, type="closed", length=arrowsize) 
        end <- list(segmentsGrob(lastCP[1], lastCP[2],
                                 getX(ep(edge)), getY(ep(edge)),
                                 default.units="native",
                                 arrow=arrow,
                                 gp=gpar(col=col, fill=col,
                                     lwd=edge@lwd, lty=edge@lty)))
    } else if (arrowhead == "open") {
        arrow <- arrow(angle=15, type="open", length=arrowsize) 
        end <- list(segmentsGrob(lastCP[1], lastCP[2],
                                 getX(ep(edge)), getY(ep(edge)),
                                 default.units="native",
                                 arrow=arrow,
                                 gp=gpar(col=col, fill=col,
                                     lwd=edge@lwd, lty=edge@lty)))
    } else {
        line <- segmentsGrob(lastCP[1], lastCP[2],
                             getX(ep(edge)), getY(ep(edge)),
                             default.units="native",
                             gp=gpar(col=col, fill=col,
                                 lwd=edge@lwd, lty=edge@lty))
        # Draw "arrowhead" by hand
        arrow <- manualArrow(lastCP[1], lastCP[2],
                             getX(ep(edge)), getY(ep(edge)),
                             col, edge@lwd, edge@lty, arrowhead, arrowsize)
        end <- list(line, arrow)
    }
    gTree(children=do.call("gList", c(curves, end)))
}

drawEdge <- function(edge, arrowlen) {
    grid.draw(makeEdge(edge, arrowlen))
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
        lapply(AgEdge(rag), drawEdge, arrowlen)
        lapply(AgNode(rag), drawNode)
    } else {
        lapply(AgNode(rag), drawNode)
        lapply(AgEdge(rag), drawEdge, arrowlen)
    }
    upViewport(2)
}
