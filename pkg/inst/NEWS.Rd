\name{NEWS}
\title{NEWS file for the gridGraphviz package}
\encoding{UTF-8}

\section{Changes in version 0.3}{
  \itemize{

    \item Added support for "ellipse" node shape

    \item Removed 'phantom edges' from undirected graphs

    \item Added agopenSized() function to create Ragraph objects whose
    size is determined by graphviz
    
    \item agopenSized also passes edge weight information through properly, 
    resulting in nicer looking graphs
    
    \item Added graphWidth and graphHeight functions to get graph dimension 
    details easily for plotting

    \item Plot Ragraph objects at their specified size rather than fitting to
    the graphics device
    
    \item Added support for triangle, pentagon, hexagon, septagon, octagon, 
    diamond and square node shapes

    \item Added support for edge labels

    \item Added support for dot/odot and box/obox arrow types
    
    \item Arrow length for "open" and "closed" arrows set to graphviz default, 
    now handles arrowsize better
    
    \item Improved naming of grobs

    \item Named the graph viewports
  }
}

\section{Changes in version 0.2}{
  \itemize{

    \item Handle node 'col' or 'fill' of ""

    \item Handle arrowsize more sensibly

    \item Support for arrowhead="dot"

    \item New argument 'nodesOnTop' for grid.graph() so that user can
    control whether nodes or edges are drawn last (on top).
  }
}

