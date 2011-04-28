
# Functions to create an SVG graphics device object, complete with
# "methods" for performing all necessary graphical operations

# This is designed to foreshadow the time when graphics devices
# in R (or at least in grid) are R objects and graphics functions
# include the device as an argument (i.e., no longer have
# the notion of graphics always going to the "current device")

# This will not be called in this way yet (instead, I will just
# be running down the grid display list and calling appropriate
# methods from that, BUT I thought it was worth designing for the
# future anyway.

# In another forward-looking move, I will create the device class
# and methods for it using S4 methods

#################
# Utility functions
#################

devParNameToSVGStyleName <- function(name) {
  switch(name,
         col="stroke",
         fill="fill",
         # Ignore changes in font (!!)
         font="fontplaceholder",
         fontsize="font-size",
         alpha="opacity",
         lty="stroke-dasharray",
         lwd="stroke-width",
         lineend="stroke-linecap",
         linejoin="stroke-linejoin",
         linemitre="stroke-miterlimit",
         NA)
}

# R lwd is in points
devLwdToSVG <- function(lwd, dev) {
    paste(lwd/72*dev@res, "px", sep="")
}

# An R lty has to become an SVG stroke-dasharray
# This is going to be imperfect (to say the least)
devLtyToSVG <- function(lty, lwd, dev) {
    # Convert lty to numeric vec
    numlty <- switch(lty,
                     solid=0,
                     # These numbers taken from ?par
                     dashed=c(4, 4),
                     dotted=c(1, 3),
                     dotdash=c(1, 3, 4, 3),
                     longdash=c(7, 3),
                     twodash=c(2, 2, 6, 2),
                     # Otherwise we're a hex string
                     as.numeric(as.hexmode(strsplit(lty, "")[[1]])))
    # Scale by lwd
    scaledlty <- numlty * lwd
    # Convert to SVG stroke-dasharray string
    paste(ifelse(scaledlty == 0,
                 "none",
                 paste(scaledlty/72*dev@res, "px", sep="")),
          collapse=",")
}

devColToSVG <- function(col) {
  if (length(col) > 1)
    warning("Only first colour used")
  # Handle "transparent" as a special case
  if (col == "transparent")
      "none"
  else {
      rgbaCol <- col2rgb(col, alpha = TRUE)
      strokeOpacity <- rgbaCol[4] / 255 # Scaling from [0,255] to [0,1]
      rgbCol <- rgbaCol[-4]
      paste("rgb(", paste(rgbCol, collapse=","), "); ",
            "stroke-opacity: ", strokeOpacity, sep="")
  }
}

devFillToSVG <- function(col) {
  if (length(col) > 1)
    warning("Only first colour used")
  # Handle "transparent" as a special case
  if (col == "transparent")
      "none"
  else {
      rgbaCol <- col2rgb(col, alpha = TRUE)
      fillOpacity <- rgbaCol[4] / 255 # Scaling from [0,255] to [0,1]
      rgbCol <- rgbaCol[-4]
      paste("rgb(", paste(rgbCol, collapse=","), "); ",
            "fill-opacity: ", fillOpacity, sep="")
  }
}

devFontSizeToSVG <- function(fontsize, dev) {
    paste(fontsize/72*dev@res, "px", sep="")
}

devLineJoinToSVG <- function(linejoin, dev) {
    # Only need to change spelling of mitre, SVG takes american form
    if (linejoin == "mitre")
        "miter"
    else
        linejoin
}

devParToSVGPar <- function(name, par, dev) {
  if (is.null(par))
    "none"
  else {
      ifelse(is.na(par),
             "none",
             switch(name,
                    col=devColToSVG(par),
                    fill=devFillToSVG(par),
                    fontsize=devFontSizeToSVG(par, dev),
                    lwd=devLwdToSVG(par, dev),
                    linejoin=devLineJoinToSVG(par, dev),
                    # By default just pass through the actual value
                    # e.g., lty has already been converted at this point
                    par))
  }
}

devParToSVGStyle <- function(gp, dev) {
    if (is.null(gp))
        result <- svgStyle()
    else {
        result <- list()
        # convert "cex" into "fontsize"
        if ("cex" %in% names(gp)) {
            if ("fontsize" %in% names(gp))
                gp$fontsize <- (gp$fontsize * gp$cex)
            else
                gp$fontsize <- (12 * gp$cex)
        }
        # Scale lty by lwd
        if ("lty" %in% names(gp)) {
            if ("lwd" %in% names(gp)) {
                gp$lty <- devLtyToSVG(gp$lty, gp$lwd, dev)
            } else {
                gp$lty <- devLtyToSVG(gp$lty, 1, dev)
            }
        }
        for (i in names(gp))
            if (!is.na(devParNameToSVGStyleName(i)))
                result[[devParNameToSVGStyleName(i)]] <-
                    devParToSVGPar(i, gp[[i]], dev)
    }
    result
}

#################
# SVG Device Stuff
#################

setClass("svgDevice",
         representation("graphicsDevice",
                        res="numeric",
                        # Object created by svgDevice() in svg.R
                        # has no S4 class yet
                        dev="ANY"))

setMethod("inchToDevX", signature(device="svgDevice"),
          function(x, device) {
            x * device@res
          })
          
setMethod("inchToDevY", signature(device="svgDevice"),
          function(x, device) {
            x * device@res
          })

setMethod("devArrow", signature(device="svgDevice"),
          function(arrow, gp, device) {
            # Angle is specified for the arrowhead in degrees, need radians
            ratAngle <- arrow$angle
            ratAngle <- ratAngle * (pi / 180)

            # We know the length, it is the adjacent line, need to find the
            # length of the opposite line for the entire arrowhead, not
            # just one half
            midpoint <- tan(ratAngle) * arrow$length
            arrowWidth <- midpoint * 2
            
            xs <- unit.c(unit(0, "inches"), arrow$length, unit(0, "inches"))
            ys <- unit.c(unit(0, "inches"), midpoint, arrowWidth)
            x <- cx(xs, device)
            y <- cy(ys, device)

            svgMarker(x, y, arrow$type, arrow$ends, arrow$name,
                      devParToSVGStyle(gp, device), device@dev)
          })
          
setMethod("devLines", signature(device="svgDevice"),
          function(lines, gp, device) {
            svgLines(lines$x, lines$y, lines$name, lines$arrow,
                     listToSVGAttrib(lines$attributes),
                     devParToSVGStyle(gp, device), device@dev)
          })

setMethod("devPolygon", signature(device="svgDevice"),
          function(polygon, gp, device) {
            svgPolygon(polygon$x, polygon$y, polygon$name,
                       listToSVGAttrib(polygon$attributes),
                       devParToSVGStyle(gp, device), device@dev)
          })

setMethod("devPath", signature(device="svgDevice"),
          function(path, gp, device) {
            svgPath(path$x, path$y, path$rule, path$name, path$arrow,
                    listToSVGAttrib(path$attributes),
                    devParToSVGStyle(gp, device), device@dev)
          })

setMethod("devRaster", signature(device="svgDevice"),
          function(raster, gp, device) {
            svgRaster(raster$x, raster$y, raster$width, raster$height,
                      raster$name, raster$just, raster$vjust, raster$hjust, listToSVGAttrib(raster$attributes),
                      devParToSVGStyle(gp, device), device@dev)
          })

setMethod("devRect", signature(device="svgDevice"),
          function(rect, gp, device) {
            svgRect(rect$x, rect$y, rect$width, rect$height, rect$name,
                    listToSVGAttrib(rect$attributes),
                    devParToSVGStyle(gp, device), device@dev)
          })

setMethod("devText", signature(device="svgDevice"),
          function(text, gp, device) {
            # Draw SVG text with no border and fill = col
            if (is.null(gp$col))
              gp$fill <- "black"
            else
              gp$fill <- gp$col
            gp$col <- NA

            svgText(text$x, text$y, text$text,
                    text$hjust, text$vjust, text$rot,
                    text$lineheight, text$charheight, text$name,
                    listToSVGAttrib(text$attributes),
                    devParToSVGStyle(gp, device), device@dev)
          })

setMethod("devCircle", signature(device="svgDevice"),
          function(circle, gp, device) {
            svgCircle(circle$x, circle$y, circle$r, circle$name,
                      listToSVGAttrib(circle$attributes),
                      devParToSVGStyle(gp, device), device@dev)
          })

setMethod("devStartGroup", signature(device="svgDevice"),
          function(group, gp, device) {
            svgStartGroup(group$name,
                          listToSVGAttrib(group$attributes),
                          devParToSVGStyle(gp, device), device@dev)
          })

setMethod("devEndGroup", signature(device="svgDevice"),
          function(device) {
            svgEndGroup(device@dev)
          })

setMethod("devClose", signature(device="svgDevice"),
          function(device) {
            svgClose(device@dev)
          })

#################
# User Functions
#################

openSVGDev <- function(name="Rplots.svg", width=6, height=6) {
    res <- par("cra")[1]/par("cin")[1]
    # par("cra")[2]/par("cin")[2]*height))
    
    new("svgDevice",
        name=name, width=width, height=height,
        res=res,
        dev=svgOpen(name, res*width, res*height))
}
                   


