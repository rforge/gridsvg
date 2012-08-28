\name{Coordinate System Import/Export}
\alias{gridSVGCoords}
\title{
  Importing an external coordinate system
}
\description{
  This function is both a getter and a setter function for coordinate
  information imported from a plot unknown to the current R session.
}
\usage{
gridSVGCoords(newcoords = NULL)
}
\arguments{
  \item{newcoords}{
    A named list (names are viewport names) of coordinate information,
    produced by \code{\link{gridToSVG}}.
  }
}
\details{
  In order to translate between SVG coordinates and the coordinate
  system that \code{grid} understands, we first need to import the
  coordinate information exported from \code{\link{gridToSVG}}. We can
  then take the JSON representation of this coordinate information and
  import it as a named list via \code{fromJSON}. This can then
  initialise a coordinate system by passing that named list into
  \code{gridSVGCoords}.
}
\value{
  If \code{newcoords} is \code{NULL}, then we get back a named list
  representing coordinate system information.

  If we pass the named list representing a coordinate system into the
  function, we get no output.
}
\author{
  Simon Potter
}
