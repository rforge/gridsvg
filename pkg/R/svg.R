
svgOpen <- function(filename="Rplots.svg", width=200, height=200) {
  # For viewing using Adobe SVG Viewer in IE
  # OR in Firefox 3 (native support)
  # create a "wrapper" html file
    # NOTE that for including plotmath output (as MathML), may
    # need to use the right sort of headers.
    # See ~/Research/Rstuff/SVG/PlotMath/README for notes from some
    # experiments AND email from David Scott that contains an
    # example from org-babel output 2011-11-01
  htmlfile <- file(paste(filename, ".html", sep=""), "w")
  # NOTE that different browsers prefer different approaches
  # See email from David Scott 2011-11-03 for some sample code
  cat(paste('<object data="', filename, '" type="image/svg+xml"',
            ' width="', ceiling(width), 'px" height="', ceiling(height), 'px"> </object>\n',
            sep=''), file=htmlfile)
  close(htmlfile)
  svgdev <- svgDevice(file(filename, 'w'), width, height)
  svgHeader(width, height, svgdev)
  return(svgdev)
}

svgClose <- function(svgdev) {
  # Test whether these vars exists, if we're building up an SVG doc using
  # the internal svg* functions then these vars may not exist.
  export.coords <- if (exists("export.coords", envir = .gridSVGEnv))
                     get("export.coords", envir = .gridSVGEnv)    
                   else
                     "none"
  export.js <- if (exists("export.js", envir = .gridSVGEnv))
                 get("export.js", envir = .gridSVGEnv)    
               else
                 "none"
  # See if we need to write out coords info at all
  if (export.coords != "none")
    svgCoords(export.coords, svgdev)
  if (export.js != "none")
    svgJSUtils(export.js, svgdev)

  return(xmlRoot(svgDevParent(svgdev)))
}

svgJSUtils <- function(export.js, svgdev) {
  # Kinda clunky, but we're grabbing the filename of the SVG device
  # and appending to it
  utilsFn <- paste(summary(svgDevFile(svgdev))$description,
                   ".convert.js", sep="")
  utilsFile <- file(system.file("js/convert.js", package = "gridSVG"))
  utilsLines <- readLines(utilsFile)
  close(utilsFile)
  if (export.js == "file") {
    destFile <- file(utilsFn)
    writeLines(utilsLines, destFile)
    close(destFile)
    catsvg(paste('<script type="application/ecmascript" xlink:href="',
                 utilsFn,
                 '"></script>\n', sep=""), svgdev)
  }

  if (export.js == "inline") {
    catsvg(paste('<script type="application/ecmascript">\n',
                 paste('<![CDATA[\n',
                       paste(utilsLines, collapse = "\n"), '\n',
                       '  ]]>\n',
                       sep=""),
                 '</script>\n', sep=""), svgdev)
  }
}

svgCoords <- function(export.coords, svgdev) {
  coordsJSON <- toJSON(get("vpCoords", envir = .gridSVGEnv))
  coordsJSON <- paste("var gridSVGCoords = ", coordsJSON, ";", sep = "")

  if (export.coords == "file") {
    # Kinda clunky, but we're grabbing the filename of the SVG device
    # and appending to it
    coordsFn <- paste(summary(svgDevFile(svgdev))$description,
                      ".coords.js", sep="")
    coordsFile <- file(coordsFn, "w")
    cat(coordsJSON, "\n", file = coordsFile, sep = "")
    close(coordsFile)
    catsvg(paste('<script type="application/ecmascript" xlink:href="',
                 coordsFn,
                 '"></script>\n', sep=""), svgdev)
  }

  if (export.coords == "inline") {
    catsvg(paste('<script type="application/ecmascript">\n',
                 paste('<![CDATA[\n',
                       coordsJSON, '\n',
                       '  ]]>\n',
                       sep=""),
                 '</script>\n', sep=""), svgdev)
  }
}

svgClipPath <- function(id, vpx, vpy, vpw,
                        vph, svgdev=svgDevice()) {
  splitID <- strsplit(id, ".", fixed = TRUE)[[1]]
  usageNumber <- as.numeric(tail(splitID, 1))
  if (usageNumber > 1)
    return()

  clipPathID <- paste(baseGrobName(id), "clipPath", sep=".")
  newXMLNode("defs", parent = svgDevParent(svgdev),
             newXMLNode("clipPath",
                        attrs = list(id = clipPathID),
                        newXMLNode("rect",
                                   attrs = list(x = round(vpx, 2),
                                                y = round(vpy, 2),
                                                width = round(vpw, 2),
                                                height = round(vph, 2),
                                                fill = "none",
                                                stroke = "none"))))
}

svgClipAttr <- function(id, clip) {
  if (clip)
    list("clip-path" = paste0("url(#", baseGrobName(id), ".clipPath)"))
  else
    list()
}

svgStartGroup <- function(id=NULL, clip=FALSE,
                          attributes=svgAttrib(), links=NULL,
                          style=svgStyle(), coords=NULL, svgdev=svgDevice()) {
  # If this is a viewport that we're starting a group for
  # we will have coordinate information, otherwise don't bother.
  if (! is.null(coords)) {
    currVpCoords <- get("vpCoords", envir = .gridSVGEnv)
    currId <- baseGrobName(getid(id, svgdev))
    currVpCoords[[currId]] <- coords
    assign("vpCoords", currVpCoords, envir = .gridSVGEnv)
  }

  has.link <- hasLink(links[id])
  if (has.link)
    svgStartLink(links[id], svgdev)
  
  attrlist <- list(id = getid(id, svgdev),
                   svgAttribTxt(attributes, id),
                   svgClipAttr(id, clip),
                   svgStyleAttributes(style))
  attrlist <- attrList(attrlist)
  newparent <- newXMLNode("g", parent = svgDevParent(svgdev),
                          attrs = attrlist)
  svgDevChangeParent(newparent, svgdev)
}

svgEndGroup <- function(id=NULL, links=NULL, svgdev=svgDevice()) {
  # In the case where we've got a link on our group, set the parent
  # one level up because we've got an "a" tag above the group
  has.link <- hasLink(links[id])
  if (has.link)
    svgEndLink(svgdev)

  svgDevChangeParent(xmlParent(svgDevParent(svgdev)), svgdev)
}

svgStartLink <- function(href="", svgdev=svgDevice()) {
  link <- newXMLNode("a",
                     parent = svgDevParent(svgdev),
                     attrs = list("xlink:href" = href))
  svgDevChangeParent(link, svgdev)
}

svgEndLink <- function(svgdev=svgDevice()) {
  parent <- xmlParent(svgDevParent(svgdev))
  svgDevChangeParent(parent, svgdev)
}

svgAnimate <- function(attrib, values,
                       begin, interp, duration, rep, revert, id=NULL, 
                       svgdev=svgDevice()) {
  n <- if (is.null(id)) 1 else length(unique(id))

  newXMLNode("animate", parent = svgDevParent(svgdev),
             attrs = list("xlink:href" = paste0("#", getid(id, svgdev, n)),
                          attributeName = attrib,
                          begin = paste0(begin, "s"),
                          calcMode = interp,
                          dur = paste0(duration, "s"),
                          values = values,
                          repeatCount = if (is.numeric(rep)) rep else if (rep) "indefinite" else 1,
                          fill = if (revert) "remove" else "freeze"))
}

# This and svgAnimateY are untested with id != NULL
# and I have a strong suspicion there may be problems
# because tapply returns a list -- see svgAnimatePoints
# for ideas for a possible solution (esp. the lpaste function)
svgAnimateXYWH <- function(attrib, values,
                           begin, interp, duration, rep, revert,
                           id=NULL,
                           svgdev=svgDevice()) {
  svgAnimate(attrib,
             paste(round(values, 2), collapse=";"),
             begin, interp, duration, rep, revert, id, svgdev)  
}

# DON'T call this with a list of length < 2!
old.lpaste <- function(alist, collapse) {
  n <- length(alist)
  if (n == 2)
    result <- paste(alist[[1]], alist[[2]])
  else 
    result <- paste(alist[[n]], lpaste(alist[1:(n-1)], collapse))
  paste(result, collapse=collapse)
}

lpaste <- function(alist, collapse) {
  n <- length(alist)
  result <- alist[[1]]
  for (i in 2:n)
    result <- paste(result, alist[[i]])
  paste(result, collapse=collapse)
}

svgAnimatePoints <- function(xvalues, yvalues, timeid,
                             begin, interp, duration, rep, revert,
                             id=NULL,
                             svgdev=svgDevice()) {
  if (is.null(id))
    warning("Only one point to animate")
  else
    svgAnimate("points",
                paste(lapply(split(paste(round(xvalues, 2),
                                         round(yvalues, 2), sep=","),
                                   timeid),
                             paste, collapse=" "),
                      collapse=";"),
               begin, interp, duration, rep, revert, id, svgdev)  
}

svgAnimatePath <- function(xvalues, yvalues, pathid, timeid,
                           begin, interp, duration, rep, revert,
                           id=NULL,
                           svgdev=svgDevice()) {
  if (is.null(id))
    warning("Not sure what this animation means?")
  else {
      # Split into time segments
      x <- split(xvalues, timeid)
      y <- split(yvalues, timeid)
      pid <- split(pathid, timeid)
      d <- mapply(function(xtime, ytime, pid) {
                      # Split into path components
                      xx <- split(xtime, pid)
                      yy <- split(ytime, pid)
                      txt <- mapply(function(x, y) {
                                        paste(paste(c("M",
                                                      rep("L", length(x) - 1)),
                                                    round(x, 2), round(y, 2),
                                                    collapse=" "),
                                              "Z")
                                    }, xx, yy)
                      paste(unlist(txt), collapse=" ")
                  }, x, y, pid)
      svgAnimate("d", paste(d, collapse=";"),
                 begin, interp, duration, rep, revert, id, svgdev)      
  }
}

svgAnimateTransform <- function(attrib, values,
                                begin, interp, duration, rep, revert,
                                id=NULL,
                                svgdev=svgDevice()) {
  n <- if (is.null(id)) 1 else length(unique(id))
  newXMLNode("animateTransform", parent = svgDevParent(svgdev),
             attrs = list("xlink:href" = paste0("#", getid(id, svgdev, n)),
                          attributeName = "transform",
                          type = attrib,
                          begin = paste0(begin, "s"),
                          calcMode = interp,
                          dur = paste0(duration, "s"),
                          values = values,
                          repeatCount = if (is.numeric(rep)) rep else if (rep) "indefinite" else 1,
                          fill = if (revert) "remove" else "freeze"))
}

svgAnimateTranslation <- function(xvalues, yvalues,
                                  begin, interp, duration, rep, revert,
                                  id=NULL,
                                  svgdev=svgDevice()) {
  svgAnimateTransform("translate",
                      paste(round(xvalues, 2),
                            round(yvalues, 2),
                            sep=",", collapse=';'),
                      begin, interp, duration, rep, revert, id, svgdev)  
}

svgAnimateScale <- function(xvalues, yvalues,
                            begin, interp, duration, rep, revert,
                            id=NULL,
                            svgdev=svgDevice()) {
  svgAnimateTransform("scale",
                      paste(round(xvalues, 2),
                            round(yvalues, 2),
                            sep=",", collapse=';'),
                      begin, interp, duration, rep, revert, id, svgdev)  
}

svgLines <- function(x, y, id=NULL, arrow = NULL,
                     attributes=svgAttrib(), links=NULL,
                     style=svgStyle(), svgdev=svgDevice()) {
  # Grabbing arrow info for marker element references
  if (! is.null(arrow$ends))
      lineMarkerTxt <- markerTxt(arrow$ends, id)
  else
      lineMarkerTxt <- NULL

  # Never fill a line
  style$fill <- "none"

  has.link <- hasLink(links[id])
  if (has.link)
    svgStartLink(links[id], svgdev)

  attrlist <- list(svgAttribTxt(attributes, id),
                   lineMarkerTxt,
                   svgStyleAttributes(style),
                   id = id,
                   points = paste0(round(x, 2), ",",
                                   round(y, 2),
                                   collapse=" "))
  attrlist <- attrList(attrlist)
  newXMLNode("polyline", parent = svgDevParent(svgdev),
             attrs = attrlist)

  if (has.link)
    svgEndLink(svgdev)
}

svgMarker <- function(x, y, type, ends, name,
                      style=svgStyle(), svgdev=svgDevice()) {
    width <- max(x)
    height <- max(y)
    if (length(x) != length(y))
        stop("x and y must be same length")
    if (is.atomic(x)) {
        if (is.atomic(y)) {
            x <- list(x)
            y <- list(y)
        } else {
            stop("'x' and 'y' must both be lists or both be atomic")
        }
    }

    d <- mapply(
                function(subx, suby) {
                    openPath <- paste(c("M",
                                      rep("L", length(subx) - 1)),
                                      round(subx, 2), round(suby, 2),
                                      collapse=" ")
                    if (type == 2) # Closed arrow
                      paste(openPath, "Z")
                    else
                      openPath
                }, x, y)

    # If the arrow is open, we don't want to fill it
    if (type == 1)
        style$fill <- "none"

    # [[1]] and [1]: markerStart
    # [[2]] and [2]: markerEnd
    # pathattrs is simply a list where each element
    # is a list that we can simply pass in as attrs
    # to newXMLNode
    ids <- markerName("both", name)
    refXs <- round(c(-width, width), 2)
    refYs <- round(c(-height / 2, height / 2), 2)
    pathlist <- attrList(list(svgStyleAttributes(style),
                              d = d))
    pathattrs <- list(pathlist,
                      pathlist)
    pathattrs[[1]]$transform <- "rotate(180)"

    newXMLNode("defs", parent = svgDevParent(svgdev),
               newXMLNode("marker",
                          attrs = list(id = ids[1],
                                       refX = refXs[1],
                                       refY = refYs[1],
                                       overflow = "visible",
                                       markerUnits = "userSpaceOnUse",
                                       markerWidth = round(width, 2),
                                       markerHeight = round(height, 2),
                                       orient = "auto"),
                          newXMLNode("path", attrs = pathattrs[[1]])),
               newXMLNode("marker",
                          attrs = list(id = ids[2],
                                       refX = refXs[2],
                                       refY = refYs[2],
                                       overflow = "visible",
                                       markerUnits = "userSpaceOnUse",
                                       markerWidth = round(width, 2),
                                       markerHeight = round(height, 2),
                                       orient = "auto"),
                          newXMLNode("path", attrs = pathattrs[[2]])))
}

markerTxt <- function(ends, name) {
    mname <- markerName(ends, name)

    if (ends == "first")
        lmt <- list("marker-start" = paste0("url(#", mname, ")"))
    if (ends == "last")
        lmt <- list("marker-end" = paste0("url(#", mname, ")"))
    if (ends == "both")
        lmt <- list("marker-start" = paste0("url(#", mname[1], ")"),
                    "marker-end" = paste0("url(#", mname[2], ")"))
    lmt
}

markerName <- function(ends, name) {
    if (ends == "first")
        mname <- paste(name, ".markerStart", sep="")
    if (ends == "last")
        mname <- paste(name, ".markerEnd", sep="")
    if (ends == "both")
        mname <- c(paste(name, ".markerStart", sep=""),
                   paste(name, ".markerEnd", sep=""))
    mname
}

svgPolygon <- function(x, y, id=NULL,
                       attributes=svgAttrib(), links=NULL,
                       style=svgStyle(), svgdev=svgDevice()) {
  if (length(x) != length(y))
    stop("x and y must be same length")

  has.link <- hasLink(links[id])
  if (has.link)
    svgStartLink(links[id], svgdev)

  tmpattr <- list(svgAttribTxt(attributes, id),
                  svgStyleAttributes(style),
                  id = id,
                  points = paste0(round(x, 2), ",",
                                  round(y, 2),
                                  collapse = " "))
  tmpattr <- attrList(tmpattr)
  newXMLNode("polygon", parent = svgDevParent(svgdev),
             attrs = tmpattr)

  if (has.link)
    svgEndLink(svgdev)
}

# Differs from polygon because it can have sub-paths
svgPath <- function(x, y, rule, id=NULL,
                    attributes=svgAttrib(), links=NULL,
                    style=svgStyle(), svgdev=svgDevice()) {
    if (length(x) != length(y))
        stop("x and y must be same length")
    if (is.atomic(x)) {
        if (is.atomic(y)) {
            x <- list(x)
            y <- list(y)
        } else {
            stop("'x' and 'y' must both be lists or both be atomic")
        }
    }
    n <- length(x)
    d <- mapply(function(subx, suby) {
                    paste(paste(c("M",
                                  rep("L", length(subx) - 1)),
                                round(subx, 2), round(suby, 2),
                                collapse=" "),
                          "Z")
                }, x, y)

    tmpattr <- list(svgAttribTxt(attributes, id),
                    svgStyleAttributes(style),
                    id = id,
                    d = paste(unlist(d), collapse = " "),
                    "fill-rule" = switch(rule, winding="nonzero", "evenodd"))
    tmpattr <- attrList(tmpattr)


    has.link <- hasLink(links[id])
    if (has.link)
        svgStartLink(links[id], svgdev)

    newXMLNode("path", parent = svgDevParent(svgdev),
               attrs = tmpattr)

    if (has.link)
        svgEndLink(svgdev)
}

svgRaster <- function(x, y, width, height, id=NULL,
                      just, vjust, hjust,
                      attributes=svgAttrib(), links=NULL,
                      style=svgStyle(), svgdev=svgDevice()) {
  # Need to extract the original grob name in order to link to the image
  grobname <- baseGrobName(id)
  fileloc <- paste(grobname, ".png", sep = "")

  has.link <- hasLink(links[id])
  if (has.link)
    svgStartLink(links[id], svgdev)

  attrlist <- list(id = id,
                   svgAttribTxt(attributes, id),
                   svgStyleAttributes(style))
  attrlist <- attrList(attrlist)
  newXMLNode("g", parent = svgDevParent(svgdev),
             attrs = attrlist,
             newXMLNode("g",
                        attrs = list(id = paste(id, "scale", sep="."),
                                     transform = paste0("scale(",
                                                        round(width, 2), ", ",
                                                        round(-height, 2), ")")),
                        newXMLNode("image",
                                   attrs = list(x = 0,
                                                y = 0,
                                                width = 1,
                                                height = 1,
                                                "xlink:href" = fileloc,
                                                preserveAspectRatio = "none"))))

  if (has.link)
    svgEndLink(svgdev)
}

svgRect <- function(x, y, width, height, id=NULL,
                    attributes=svgAttrib(), links=NULL,
                    style=svgStyle(), svgdev=svgDevice()) {
  has.link <- hasLink(links[id])
  if (has.link)
    svgStartLink(links[id], svgdev)

  attrlist <- list(id = id,
                   x = round(x, 2),
                   y = round(y, 2),
                   width = round(width, 2),
                   height = round(height, 2),
                   svgAttribTxt(attributes, id),
                   svgStyleAttributes(style))
  attrlist <- attrList(attrlist)
  newXMLNode("rect", parent = svgDevParent(svgdev),
             attrs = attrlist)

  if (has.link)
    svgEndLink(svgdev)
}

svgTextSplitLines <- function(text, lineheight, charheight,
                              vjust, svgdev) {
    # Splitting based on linebreaks
    splitText <- strsplit(text, "\n")
    # If text is "", produces character(0), so fix that
    if (length(splitText[[1]]) == 0)
        splitText[[1]] <- ""

    n <- length(splitText[[1]])

    # Need to adjust positioning based on vertical justification.
    # Horizontal justification is done for us.
    # Only the first line needs to be modified, the rest are all
    # just one line below the previous line
    if (vjust %in% c("centre", "center"))
        firstDelta <- - ((lineheight * (n - 1) - charheight) / 2)
    if (vjust == "bottom")
        firstDelta <- - (n - 1) * lineheight
    if (vjust == "top")
        firstDelta <- charheight
    lineheight <- c(firstDelta, rep(lineheight, n - 1))

    textContent <- splitText[[1]]
    # Note that x=0 here so that we push it to the left, hjust
    # is worked out automatically from there
    for (i in 1:n) {
        newXMLNode("tspan", parent = svgDevParent(svgdev),
                   attrs = list(dy = round(lineheight[i], 2),
                                x = 0),
                   newXMLTextNode(textContent[i]))
    }
}

svgTextElement <- function(text, rot, hjust, vjust,
                           lineheight, charheight, style, svgdev=svgDevice()) {
    # Rotation in SVG goes clockwise from +ve x=axis
    transform <- if (rot != 0)
                   list(transform = paste0("rotate(", round(-rot, 2), ")"))
                 else
                   NULL
    attrlist <- list(x = 0,
                     y = 0,
                     transform,
                     textAnchor(hjust),
                     svgStyleAttributes(style))
    attrlist <- attrList(attrlist)
    newpar <- newXMLNode("text", parent = svgDevParent(svgdev),
                         attrs = attrlist)
    # Set parent of all <tspan>s to be the <text> el
    svgDevChangeParent(newpar, svgdev)
    # Write each of the lines here
    svgTextSplitLines(text, lineheight, charheight, vjust, svgdev)
    # Resetting parent
    svgDevChangeParent(xmlParent(newpar), svgdev)
}

# NOTE that the precise placement of math is even less likely to work
# than normal text.  Besides the problem of the browser using a
# different font (which is more likely because a math expression
# typically uses multiple fonts), the web browser will be using
# a different formula layout engine compared to R so things like
# the spacing between operators will be different.
# One particular problem is that R justifies math formulas
# relative to the bounding box of the formula, whereas it
# appears that Firefox at least justifies relative to the formula
# baseline (just from observation).
# The code below tries to do something rational by making use
# of finer detail metric information for the formula
# to mimic R's vertical justification.  
svgMathElement <- function(text, rot, hjust, vjust,
                           width, height, ascent, descent,
                           lineheight, charheight, fontheight,
                           fontfamily, fontface, style,
                           svgdev=svgDevice()) {
    # Determine x/y based on width/height and hjust/vjust
    if (hjust %in% c("centre", "center"))
        x <- -width/2
    if (hjust == "left")
        x <- 0
    if (hjust == "right")
        x <- -width
    if (vjust %in% c("centre", "center"))
        y <- -(max(ascent, fontheight) + descent)/2
    if (vjust == "bottom")
        y <- -(max(ascent, fontheight) + descent)
    if (vjust == "top") {
        if (fontheight > ascent)
            y <- -(fontheight - ascent)
        else
            y <- (ascent - fontheight)
    }

    tmpattr <- list(x = round(x, 2),
                    y = round(y, 2),
                    width = round(3*width, 2),
                    height = round(3*height, 2),
                    svgStyleAttributes(style))
    if (rot != 0)
        tmpattr$transform <- paste0("rotate(", round(-rot, 2), ")")

    foreignObj <- newXMLNode("foreignObject", parent = svgDevParent(svgdev),
                             attrs = tmpattr)
    svgDevChangeParent(foreignObject, svgdev)
    exprNode <- expr2mml(text, fontfamily, fontface, svgdev)
    svgDevChangeParent(xmlParent(foreignObj), svgdev)

   # # Adjust exact width/height up by large fudge factor to allow for
   # # larger fonts and different layout in viewer
   # # Hopefully there are no downsides to that approach ...
   # paste('<foreignObject x="', round(x, 2),
   #                    '" y="', round(y, 2),
   #                    '" width="', round(3*width, 2),
   #                    '" height="', round(3*height, 2), '" ',
   #       if (rot != 0) {
   #           paste('transform="rotate(',
   #                 # Rotation in SVG goes clockwise from +ve x=axis
   #                 round(-rot, 2),
   #                 ')" ', sep="")
   #       } else "",
   #       svgStyleAttributes(style),
   #       ' >\n',
   #       expr2mml(text, fontfamily, fontface),
   #       '</foreignObject>\n',
   #       sep="")
}

svgText <- function(x, y, text, hjust="left", vjust="bottom", rot=0,
                    width=1, height=1, ascent=1, descent=0,
                    lineheight=1, charheight=.8, fontheight=1,
                    fontfamily="sans", fontface="plain",
                    id=NULL, attributes=svgAttrib(), links=NULL,
                    style=svgStyle(), svgdev=svgDevice()) {
    has.link <- hasLink(links[id])
    if (has.link)
        svgStartLink(links[id], svgdev)

    topattrs <- svgAttribTxt(attributes, id)
    topattrs$transform <- paste0("translate(",
                                 round(x, 2), ", ",
                                 round(y, 2), ")")
    topattrs$`stroke-width` <- "0.1px"

    # Flip the y-direction again so that text is drawn "upright"
    # Do the flip in a separate <g> so that can animate the
    # translation easily
    # Use a tspan to do the vertical alignment
    topg <- newXMLNode("g", parent = svgDevParent(svgdev),
                       attrs = topattrs)
    sec <- newXMLNode("g", parent = topg,
                      attrs = list(transform = "scale(1, -1)"))

    # Let all child <tspan> elements or MathML fragments be
    # located under the *second* <g>
    svgDevChangeParent(sec, svgdev)

    if (is.language(text)) {
        svgMathElement(text, rot, hjust, vjust,
                       width, height, ascent, descent,
                       lineheight, charheight, fontheight,
                       fontfamily, fontface, style,
                       svgdev)
    } else {
        svgTextElement(text, rot, hjust, vjust,
                       lineheight, charheight, style,
                       svgdev)
    }

    # Reset parent to parent of entire text "grob"
    svgDevChangeParent(xmlParent(topg), svgdev)

    if (has.link)
        svgEndLink(svgdev)
}

svgCircle <- function(x, y, r, id=NULL,
                      attributes=svgAttrib(), links=NULL,
                      style=svgStyle(), svgdev=svgDevice()) {
  has.link <- hasLink(links[id])
  if (has.link)
    svgStartLink(links[id], svgdev)

  tmpattr <- list(id = id,
                  cx = round(x, 2),
                  cy = round(y, 2),
                  r = round(r, 2),
                  svgAttribTxt(attributes, id),
                  svgStyleAttributes(style))
  tmpattr <- attrList(tmpattr)
  has.link <- hasLink(links[id])
  newXMLNode("circle", parent = svgDevParent(svgdev),
             attrs = tmpattr)

  if (has.link)
    svgEndLink(svgdev)
}

svgScript <- function(body, href, type="application/ecmascript",
                      id=NULL, svgdev=svgDevice()) {
  tmpattr <- list(type = type,
                  id = getid(id, svgdev, 1))
  if (nchar(href) > 0)
    tmpattr$`xlink:href` <- href

  script <- newXMLNode("script", parent = svgDevParent(svgdev),
                       attrs = tmpattr)

  if (nchar(body) > 0) {
    # "body" adds newlines because otherwise the CDATA delimiters are part
    # of the first and last line of text, break it apart to look nicer
    newXMLCDataNode(paste0("\n", body, "\n"),
                    parent = script)
  }
}

#############
# Internal functions
#############

# SVG Devices
# A device is an environment so that we can modify values
# stored within it.
# Store a list of transformation functions for
# x, y, width, and height;  this will allow viewports
# to be defined within user coordinates (see svgPushViewport
# and svgPopViewport)

svgDevice <- function(file="", width=200, height=200) {
  dev <- new.env(FALSE, emptyenv())
  assign("file", file, envir=dev)
  assign("width", width, envir=dev)
  assign("height", height, envir=dev)
  assign("parent", NULL, envir=dev)
  assign("indent", "", envir=dev)
  assign("id", 1, envir=dev)
  return(dev)
}

svgDevFile <- function(svgdev) {
  get("file", envir=svgdev)
}

svgDevWidth <- function(svgdev) {
  get("width", envir=svgdev)
}

svgDevHeight <- function(svgdev) {
  get("height", envir=svgdev)
}

svgDevParent <- function(svgdev) {
  get("parent", envir=svgdev)
}

svgDevChangeParent <- function(newpar, svgdev) {
  assign("parent", newpar, envir=svgdev)
}

getid <- function(id, svgdev, n=1) {
  if (is.null(id))
    svgID(svgdev) + (1:n - 1)
  else {
    if (n > 1)
      paste(id, 1:n, sep="")
    else
      id
  }
}

svgID <- function(svgdev) {
  get("id", envir=svgdev)
}

hasLink <- function(link) {
  ! (is.null(link) || is.na(link))
}

# SVG output
# This guy can optionally add an <a> element around the output
catsvg <- function(node, svgdev, link=NULL) {
    #hasLink <- !(is.null(link) || is.na(link))
    #if (hasLink) {
    #    svgStartLink(link, svgdev)
    #}
    #cat(paste(get("indent", envir=svgdev), text, sep=""),
    #    file=svgDevFile(svgdev))
    #if (hasLink) {
    #    svgEndLink(svgdev)
    #}
}

decindent <- function(svgdev) {
  indent <- get("indent", envir=svgdev)
  assign("indent", substr(indent, 1, nchar(indent) - 2),
         envir=svgdev)
}

incindent <- function(svgdev) {
  assign("indent", paste(get("indent", envir=svgdev), "  ", sep=""),
         envir=svgdev)
}

incID <- function(svgdev, n=1) {
  assign("id", get("id", envir=svgdev) + n, envir=svgdev)
}

svgHeader <- function(width, height, svgdev=svgDevice()) {
    # This header tested on standalone SVG file in Firefox 3
    # FIXME:  add default xmlns for animation and scripts too?
    svgdoc <- newXMLNode("svg",
                         namespaceDefinitions = list("http://www.w3.org/2000/svg",
                                                     xlink = "http://www.w3.org/1999/xlink"),
                         attrs = list(width = paste0(round(width, 2), "px"),
                                      height = paste0(round(height, 2), "px"),
                                      version = "1.1"))
    # Invert the y-axis so that y and height values measure "up"
    rootg <- newXMLNode("g",
                        parent = svgdoc,
                        attrs = list(transform = paste0("translate(0, ",
                                                        round(svgDevHeight(svgdev), 2),
                                                        ") scale(1, -1)")))
    svgDevChangeParent(rootg, svgdev)
}

svgFooter <- function(svgdev=svgDevice()) {}

# SVG attributes
svgAttrib <- function(...) {
  temp <- list(...)
  if (length(temp) == 0)
    list()
  else if (is.null(temp[[1]]))
    list()
  else
    temp
}

# Removes NULL values and flattens our attrib list
# so we can include lists as elements in "alist"
# and arrive at a flattened list
attrList <- function(alist) {
  as.list(unlist(alist))
}

listToSVGAttrib <- function(alist) {
  alist
}

emptyAttrib <- function(attributes) {
  length(attributes) == 0
}

# Only use the attributes that are for this 'id'
svgAttribTxt <- function(attributes, id) {
    if (emptyAttrib(attributes)) {
        list()
    } else {
        attributes <- lapply(attributes,
                             function(attr, id) {
                                 kept <- attr[names(attr) == id]
                                 if (length(kept) == 0)
                                     NULL
                                 else
                                     kept
                             },
                             id)
        # Drop NULL attributes
        attributes <- attributes[!sapply(attributes, is.null)]

        # Need to wipe out names because it messes things up when we
        # need to create an attribute list for nodes
        if (length(attributes) > 0)
            lapply(attributes, function(x) {
                       names(x) <- NULL
                       x
                   })
        else
            list()
    }
}

# SVG styling
svgStyle <- function(...) {
  list(...)
}

listToSVGStyle <- function(alist) {
  alist
}

emptyStyle <- function(svgstyle) {
  length(svgstyle) == 0
}

svgStyleCSS <- function(svgstyle) {
  if (emptyStyle(svgstyle)) {
    ""
  } else {
      paste('style="',
            do.call("paste",
                    c(mapply(function(name, style) {
                        paste(name, ":", style, sep="")
                    }, names(svgstyle), svgstyle),
                      list(sep="; "))),
            '"', sep="")
    # paste('style="', paste(names(svgstyle), ":",
    #                        paste(svgstyle), sep="", collapse="; "),
    #       '"', sep="")
  }
}

# NOTE using SVG presentation attributes
# RATHER THAN CSS style attribute
# BECAUSE ...
# - can modify single presentation attribute without affecting
#   other presentation attributes (if CSS style then have to
#   reset the entire thing) and can do this from JavaScript.
# - presentation attributes have lower priority than CSS style
#   SO this allows overriding by specifying CSS style later.
#   Can also override with general style sheet later.
svgStyleAttributes <- function(svgstyle) {
    if (emptyStyle(svgstyle)) {
        list()
    } else {
        if (any(sapply(svgstyle, length) > 1))
            stop("All SVG style attribute values must have length 1")
        svgstyle
        #paste(names(svgstyle), "=\"", svgstyle, "\"", sep="", collapse=" ")
    }    
}

# Specifying text justification
textAnchor <- function(hjust) {
  list("text-anchor" = 
       switch(hjust,
              left="start",
              center="middle",
              centre="middle",
              right="end",
              "start"))
}

dominantBaseline <- function(vjust) {
  list("dominant-baseline" =
       switch(vjust,
              bottom="auto",
              center="middle",
              centre="middle",
              top="text-top",
              "baseline"))
}

baselineShift <- function(vjust) {
  list("baseline-shift" =
       switch(vjust,
              bottom="0%",
              center="-50%",
              centre="-50%",
              top="-100%",
              "0%"))
}

alignmentBaseline <- function(vjust) {
  list("alignment-baseline" =
       switch(vjust,
              baseline="baseline",
              bottom="bottom",
              center="middle",
              centre="middle",
              top="top",
              "baseline"))
}

