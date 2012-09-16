\name{grid.comment}
\alias{grid.comment}
\alias{commentGrob}
\title{ Annotate a grid grob with a comment }
\description{
  Creates a \code{commented.grob} object which is a normal grid grob,
  with a comment attached. Useful in conjunction with \code{gridToSVG},
  to produce an SVG document with comments preceding graphical elements.
}
\synopsis{
grid.comment(path, comment)
commentGrob(grob, comment)
}
\usage{
grid.comment(path, comment)
}
\arguments{
  \item{path}{ A grob path specifying a drawn grob. }
  \item{comment}{
    A character vector used to annotate the grob. If this has a length
    greater than one, each element is assumed to be a line.
  }
}
\value{
  A \code{commented.grob} object.
}
\author{ Simon Potter }
\seealso{ \code{\link{gridToSVG}} }
\keyword{ dplot }

