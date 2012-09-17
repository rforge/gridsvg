\name{viewportCreate}
\alias{viewportCreate}
\title{
  Recreate a viewport from imported coordinate information.
}
\description{
  Creates a viewport object that is positioned in the same location as a
  previously exported viewport.

  The purpose of this function is so that we can recreate content for
  later manipulation.
}
\usage{
viewportCreate(vpname)
}
\arguments{
  \item{vpname}{ The name of the viewport to be recreated. }
}
\details{
  In order to use this function, coordinate information must be
  available to \code{gridSVG}. This means that viewport information must
  be imported using \code{\link{gridSVGCoords}}.

  The \code{ROOT} viewport must also have coordinate information
  imported because the created viewport is positioned relative to this.
}
\value{
  A viewport object.
}
\author{
  Simon Potter
}
\examples{\dontrun{
  grid.newpage()

  # Pushing a new VP to draw a rect within
  pushViewport(viewport(x = unit(0.3, "npc"), y = unit(0.2, "npc"),
                        width = unit(0.1, "npc"), height = unit(0.3, "npc"),
                        xscale = c(0, 20), yscale = c(0, 10),
                        name = "testVP"))
  grid.rect()
  gridToSVG("create-test.svg")

  # Importing coordinate information
  gridSVGCoords(fromJSON(readCoordsJS("create-test.svg.coords.js")))

  # This should appear to be the same rect
  grid.newpage()
  pushViewport(viewportCreate("testVP"))
  grid.rect()

  # Let's see if the scales are accurate, should be:
  # xscale: [0, 20]
  # yscale: [0, 10]
  current.viewport()$xscale
  current.viewport()$yscale
}}
