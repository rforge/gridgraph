## create label grobs
makeLabel <- function(label, x, y, col, fontsize, cex, name) {
    textGrob(label, x, y,
             gp=gpar(col=col, fontsize=fontsize, cex=cex),
             name=paste("label", name, sep="-"))
}

## create box grob for node gTree in node()
nodeBox <- function(shape, name, x, y, height, lwidth, rwidth, color,
                    fillcolor) {
    a <- 0.5 * (lwidth + rwidth)
    b <- 0.5 * height
    boxName <- paste("box", name, sep="-")

    if (shape == "circle") {
        box <- circleGrob(x, y, r=a, name=boxName,
                          gp=gpar(col=color, fill=fillcolor))
    } else if (shape == "ellipse") { 
        angle <- seq(0, 2*pi, length=101)
        box <- polygonGrob(x + a*cos(angle),
                           y + b*sin(angle),
                           name=boxName,
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
                           name=boxName,
                           gp=gpar(col=color, fill=fillcolor))
    } else if (shape == "box" || shape == "rect" || shape == "rectangle") {
        box <- rectGrob(x, y,
                        width=lwidth + rwidth,
                        height=height,
                        name=boxName,         
                        gp=gpar(col=color, fill=fillcolor))
    } else if (shape == "square") {
        box <- rectGrob(x, y, width=height, height=height,
                        gp=gpar(col=color, fill=fillcolor))
    } else if (shape == "diamond") {
        xPoints <- unit.c(x - lwidth, x, x + rwidth, x)
        yPoints <- unit.c(y, y + b, y, y - b)
        box <- polygonGrob(xPoints, yPoints, name=boxName,
                           gp=gpar(col=color, fill=fillcolor))
    } else { # plain
        warning("Unsupported node shape; using 'box'")
        box <- rectGrob(x, y,
                        width=lwidth + rwidth,
                        height=height,
                        name=boxName, gp=gpar(col=NA, fill=NA))
    }
    box
}
