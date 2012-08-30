\name{gridToSVG}
\alias{gridToSVG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate SVG output from a grid graphic }
\description{
  Produces an SVG version of the current grid page.
}
\usage{
gridToSVG(name = "Rplots.svg",
          export.coords = c("file", "inline", "none"),
          export.js = c("file", "inline", "none"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{ The name of the SVG file to produce }
  \item{export.coords}{
    If this parameter is not \code{none} a coordinates file is exported.

    If this parameter is \code{file}, the coordinates information is
    written to a file, while \code{inline} will include the contents
    within the SVG document.
  }
  \item{export.js}{
    If this parameter is not \code{none} a JavaScript file is written out.
    This contains useful functions for manipulating gridSVG plots in the
    browser, including unit conversion functions.

    If this parameter is \code{file}, the JavaScript file is written to
    a file, while \code{inline} will include the contents within the SVG
    document.
  }
}
\details{
  See the files in the directory gridSVG/tests for examples
  of things that can be done.  See the file gridSVG/doc/overview.tex
  for limitations.
}
\value{
  None.  The side effect is to produce an SVG file of the specified
  name, AND a very rudimentary HTML file with the SVG document embedded.
  Optionally a JavaScript file containing coordinate transformation
  information is also exported. Optionally a JavaScript file containing
  utility JavaScript functions is also exported.
}
\author{ Paul Murrell }
\seealso{ \code{\link{grid.hyperlink}}, \code{\link{grid.animate}} }
\keyword{ dplot }% at least one, from doc/KEYWORDS
