
# Functions to take a grid grob and call appropriate
# functions from svg.R to produce SVG output


# User function
gridToSVG <- function(name="Rplots.svg",
                      export.coords=c("file", "inline", "none"),
                      export.js=c("file", "inline", "none")) {
    # Saving we know how to export
    export.coords <- match.arg(export.coords)
    export.js <- match.arg(export.js)
    assign("export.coords", export.coords, envir = .gridSVGEnv)
    assign("export.js", export.js, envir = .gridSVGEnv)

    # Don't do text substitution until necessary
    assign("plotmathUsed", FALSE, envir = .gridSVGEnv)

    # Ensure we're at the top level
    upViewport(0)
    rootgp <- get.gpar()
    rootvp <- current.viewport()
    roottm <- current.transform()

    svgdev <- openSVGDev(name, width=par("din")[1], height=par("din")[2])
    # Create a gTree from the current page
    # NOTE that set the 'gp' slot on this top-level gTree
    # based on ROOT vp
    # Use 'wrap=TRUE' to ensure correct capture of all types of 'grid' output
    gTree <- grid.grab(name="gridSVG", wrap=TRUE, gp=rootgp)
    # Emptying the VP usage table
    vpUsageTable <- data.frame(vpname = character(0),
                               count = integer(0),
                               stringsAsFactors=FALSE)
    assign("vpUsageTable", vpUsageTable, envir = .gridSVGEnv)
    # Because the root viewport is never entered into, we need to set
    # the root vp coordinate information before we start entering into
    # other VPs
    currVpCoords <- list(ROOT = getCoordsInfo(rootvp, roottm, svgdev))
    assign("vpCoords", currVpCoords, envir = .gridSVGEnv)

    # Convert gTree to SVG
    gridToDev(gTree, svgdev)
    svgroot <- devClose(svgdev)
    svgdoc <- xmlDoc(svgroot)
    doctxt <- saveXML(svgdoc)

    # MathML fix, XML package is escaping too much, even when we tell it
    # not to. Unescape the second level of escaping
    if (get("plotmathUsed", envir = .gridSVGEnv)) {
        doctxt <- gsub("&amp;(#x[0-9A-Fa-f]+;)", "&\\1", doctxt)
        doctxt <- gsub("&amp;gt;", "&gt;", doctxt)
        doctxt <- gsub("&amp;lt;", "&lt;", doctxt)
        doctxt <- gsub("&amp;amp;", "&amp;", doctxt)
        doctxt <- gsub("&amp;apos;", "&apos;", doctxt)
        doctxt <- gsub("&amp;quot;", "&quot;", doctxt)
    }

    cat(doctxt, file = name)

    # In an on-screen device, we can be left with a blank device
    # so refresh just to ensure we can see everything. Also happens
    # with devices like png and pdf so just force a refresh.
    # Also, to avoid having to ask to refresh, just temporarily
    # disable asking.
    old.ask <- devAskNewPage(FALSE)
    on.exit(devAskNewPage(old.ask))
    grid.refresh()
}

old.gridToSVG <- function(name="Rplots.svg") {
  svgdev <- openSVGDev(name, width=par("din")[1], height=par("din")[2])
  # Start a new page because we are going to be reproducing the
  # pushing and popping of viewports and this needs to be done
  # from scratch 
  grid.newpage(recording=FALSE)
  # Traverse the grid display list producing
  # SVG equivalents of all grid output
  # This nastily peeks into the grid NAMESPACE to get the
  # display list (for now)
  lapply(grid:::grid.Call("L_getDisplayList"), gridToDev, svgdev)
  # Before closing, need to pop the top-level viewport
  # which is not possible in grid
  devEndGroup(svgdev)
  devClose(svgdev)
}
