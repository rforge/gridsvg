
# duration says how many SECONDS the animation lasts for
# id indicates the identity of multiple animated values
#   (i.e., allows a vector of animated values)
#   If "auto" then it depends on the number and size
#     of the elements being animated.  If there is
#     only one element, it is NULL.
# rep says how many times to repeat the animation
#   (TRUE means indefinitely;  FALSE means once)
# revert says whether to revert to the start value of the
#   animation upon completion

autoid <- function(id) {
  if (!is.numeric(id))
    if (id == "auto")
      TRUE
    else
      stop("Invalid id")
  else
    FALSE
}

animateGrob <- function(grob, ...,
                        duration=1, id="auto",
                        rep=FALSE, revert=FALSE) {
  animations <- list(...)
  if (is.null(animations[[1]]))
    stop("need argument to animate")
  cl <- class(grob)
  grob$animations <- c(animations, list(duration=duration, id=id,
                    rep=rep, revert=revert))
  class(grob) <- c("animated.grob", cl)
  grob
}
  
grid.animate <- function(path, ...) {
  grid.set(path, animateGrob(grid.get(path), ...))
}

animate <- function(x, animation, dev) {
  UseMethod("animate")
}

animate.rect <- function(x, animation, dev) {

  # We may be dealing with multiple rects that need animating
  n <- max(length(x$x), length(x$y), length(x$width), length(x$height))

  # Repeating animation parameters so that each element can have
  # distinct values
  dur <- rep(x$animations$duration, length.out = n)
  rep <- rep(x$animations$rep, length.out = n)
  rev <- rep(x$animations$revert, length.out = n)

  for (i in 1:n) {
    subName <- subGrobName(x$name, i)

    switch(animation,
           x={
             if (! is.matrix(x$animations$x))
               x$animations$x <- matrix(x$animations$x)
             xunit <- attr(x$x, "unit")
             lb <- leftbottom(unit(x$animations$x[,i], xunit), x$y, x$width, x$height, x$just,
                              dev)
             svgAnimateXYWH("x", cx(lb$x, dev),
                            dur[i], rep[i], rev[i], subName, dev@dev)
           },
           y={
             if (! is.matrix(x$animations$y))
               x$animations$y <- matrix(x$animations$y)
             yunit <- attr(x$y, "unit")
             lb <- leftbottom(x$x, unit(x$animations$y[,i], yunit), x$width, x$height, x$just,
                              dev)
             svgAnimateXYWH("y", cy(lb$y, dev),
                            dur[i], rep[i], rev[i], subName, dev@dev)
           },
           width={
             if (! is.matrix(x$animations$width))
               x$animations$width <- matrix(x$animations$width)
             wunit <- attr(x$width, "unit")
             dim <- dimToInches(unit(x$animations$width[,i], wunit), x$height, dev)
             svgAnimateXYWH("width", cw(dim$w, dev),
                            dur[i], rep[i], rev[i], subName, dev@dev)
           },
           height={
             if (! is.matrix(x$animations$height))
               x$animations$height <- matrix(x$animations$height)
             hunit <- attr(x$height, "unit")
             dim <- dimToInches(x$width, unit(x$animations$height[,i], hunit), dev)
             svgAnimateXYWH("height", ch(dim$h, dev),
                            dur[i], rep[i], rev[i], subName, dev@dev)
           })
  }
}

animate.circle <- function(x, animation, dev) {

  # We may be dealing with multiple circles that need animating
  n <- max(length(x$x), length(x$y), length(x$r))

  # Repeating animation parameters so that each element can have
  # distinct values
  dur <- rep(x$animations$duration, length.out = n)
  rep <- rep(x$animations$rep, length.out = n)
  rev <- rep(x$animations$revert, length.out = n)

  # Because grobs can produce multiple elements, if animation is to
  # occur on a grob it is assumed to occur on all elements, but
  # elements may simply have their properties assigned to the same
  # value multiple times.
  #
  # Also note that when casting to a matrix, units lose their "unit"
  # attribute, we have to set this to the same unit as the grob
  # attribute that is being animated, for this reason, attributes should
  # be in the same unit prior to calling grid.animate()
  for (i in 1:n) {
    subName <- subGrobName(x$name, i)

    switch(animation,
       x={
         if (! is.matrix(x$animations$x))
           x$animations$x <- matrix(x$animations$x)
         xunit <- attr(x$x, "unit")
         loc <- locToInches(unit(x$animations$x[,i], xunit), x$y, dev)
         svgAnimateXYWH("cx", cx(loc$x, dev),
                        dur[i], rep[i], rev[i], subName, dev@dev)
       },
       y={
         if (! is.matrix(x$animations$y))
           x$animations$y <- matrix(x$animations$y)
         yunit <- attr(x$y, "unit")
         loc <- locToInches(x$x, unit(x$animations$y[,i], yunit), dev)
         svgAnimateXYWH("cy", cy(loc$y, dev),
                        dur[i], rep[i], rev[i], subName, dev@dev)
       },
       r={
         if (! is.matrix(x$animations$r))
           x$animations$r <- matrix(x$animations$r)
         runit <- attr(x$r, "unit")
         svgAnimateXYWH("r", cd(unit(x$animations$r[,i], runit), dev),
                        dur[i], rep[i], rev[i], subName, dev@dev)
       })
  }
}

animate.text <- function(x, animation, dev) {
  dur <- x$animations$duration
  rep <- x$animations$rep
  rev <- x$animations$revert
  # Special case if animating BOTH x and y
  if (all(c("x", "y") %in% names(x$animations))) {
    loc <- locToInches(x$animations$x, x$animations$y, dev)
    svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                          dur, rep, rev, x$name, dev@dev)
  } else {
    switch(animation,
           x={
             loc <- locToInches(x$animations$x, x$y, dev)
             svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                                   dur, rep, rev, x$name, dev@dev)
           },
           y={
             loc <- locToInches(x$x, x$animations$y, dev)
             svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                                   dur, rep, rev, x$name, dev@dev)
           })
  }
}

animate.lines <- function(x, animation, dev) {
  dur <- x$animations$duration
  rep <- x$animations$rep
  rev <- x$animations$revert
  # Special case if animating BOTH x and y
  if (all(c("x", "y") %in% names(x$animations))) {
    loc <- locToInches(x$animations$x, x$animations$y, dev)
    svgAnimatePoints(cx(loc$x, dev), cy(loc$y, dev), x$animations$id,
                     dur, rep, rev, x$name, dev@dev)
  } else {
    switch(animation,
           x={
             loc <- locToInches(x$animations$x, x$y, dev)

           },
           y={
             loc <- locToInches(x$x, x$animations$y, dev)

           })
  }
  
}


primToDev.animated.grob <- function(x, dev) {
  animations <- x$animations[!names(x$animations) %in%
                             c("duration", "id", "rep", "revert")]
  for (i in names(animations)) 
    animate(x, i, dev)
  NextMethod()
}

