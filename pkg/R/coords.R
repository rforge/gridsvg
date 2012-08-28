gridSVGCoordsGen <- function() {
  coords <- NULL
  function(newcoords = NULL) {
    if (is.null(newcoords)) {
      coords
    } else {
      coords <<- newcoords
    }
  }
}

gridSVGCoords <- gridSVGCoordsGen()

validCoordsInfo <- function(vpname) {
  currCoords <- gridSVGCoords()
  if (is.null(currCoords)) {
    stop("No coordinates data has been loaded.")
  } else if (is.null(currCoords[[vpname]])) {
    stop("Viewport not found in coordinates data")
  } else {
    currCoords
  }
}
