
# Add arbitrary SVG attributes to a grob

garnishGrob <- function(x, ...) {
  cl <- class(x)
  # Should check that attributes are valid
  # Will need to be generic check with per-grob-type versions
  x$attributes <- list(...)
  class(x) <- c("svg.grob", cl)
  x
}

grid.garnish <- function(path, ..., redraw=TRUE) {
  grid.set(path, garnishGrob(grid.get(path), ...), redraw=redraw)
}

devGrob.svg.grob <- function(x, dev) {
  c(NextMethod(x), attributes=list(x$attributes))
}

# General function to apply a function to each element of the
# grid display list - should end up in 'grid' itself
grid.DLapply <- function(fun, ...) {
    # Traverse DL and do something to each entry
    gridDL <- grid:::grid.Call.graphics("L_getDisplayList")
    gridDLindex <- grid:::grid.Call.graphics("L_getDLindex")
    for (i in 1:gridDLindex) {
        elt <- grid:::grid.Call.graphics("L_getDLelt", i)
        grid:::grid.Call.graphics("L_setDLindex", i)
        grid:::grid.Call.graphics("L_setDLelt", fun(elt, ...))
    }
    grid:::grid.Call.graphics("L_setDLindex", gridDLindex)
}

# Add tooltip attributes to a grob on the DL
garnishAllGrobs <- function(elt) {
    if (inherits(elt, "grob")) {
        garnishGrob(elt,
                    onmouseover=paste("showTooltip(evt, '",
                      gsub("\n", " ", elt$name), "')",
                      sep=""),
                    onmouseout="hideTooltip()")
    } else {
        elt
    }
}

