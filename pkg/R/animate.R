

#######################
# "animValue" stuff

# An animValue is a vector PLUS a timeid PLUS an id

animValue <- function(x, timeid=NULL, id=NULL) {
    if (!is.atomic(x))
        stop("'x' must be a atomic")
    if (!is.null(timeid))
        timeid <- rep(timeid, length.out=length(x))
    if (!is.null(id))
        id <- rep(id, length.out=length(x))
    tu <- list(values=x, timeid=timeid, id=id)
    class(tu) <- "animValue"
    tu
}

is.animValue <- function(x) inherits(x, "animValue")

as.animValue <- function(x, ...) {
    UseMethod("as.animValue")
}

as.animValue.animValue <- function(x, ...) x

as.animValue.numeric <- function(x, ...) {
    animValue(x)
}

as.animValue.character <- function(x, ...) {
    animValue(x)
}

# 'multVal' controls whether columns of the matrix are used as
# 'timeid' or 'id'
as.animValue.matrix <- function(x, multVal=FALSE, ...) {
    if (multVal) {
        animValue(x, timeid=rep(1:ncol(x), each=nrow(x)))
    } else {
        animValue(x, id=rep(1:ncol(x), each=nrow(x)))
    }
}

as.animValue.list<- function(x, multVal=FALSE, ...) {
    if (!all(sapply(x, is.atomic)))
        stop("All components of list must be atomic")
    if (multVal) {
        animValue(unlist(x),
                  timeid=rep(1:length(x), sapply(x, length)))
    } else {
        animValue(unlist(x),
                  id=rep(1:length(x), sapply(x, length)))
    }
}

listFromAnimValue <- function(x) {
    if (is.null(x$id)) {
        if (is.null(x$timeid)) {
            n <- length(x$values)
            animValueList <- as.list(x$values)
        } else {
            times <- unique(x$timeid)
            n <- length(times)
            animValueList <- split(x$values, x$timeid)
        }
        names(animValueList) <- paste("t", 1:n, sep="")
    } else {
        shapes <- unique(x$id)
        ns <- length(shapes)
        animValueList <- vector("list", ns)
        for (i in 1:ns) {
            animValueList[[i]] <-
                listFromAnimValue(animValue(x$values[x$id == i],
                                            x$timeid[x$id == i]))
        }
        names(animValueList) <- paste("id", 1:ns, sep="")
    }
    animValueList
}

print.animValue <- function(x, ...) {
    # Generate list from animValue and then print the list
    print(listFromAnimValue(x))
}

#######################
# "animUnit" stuff

# An animUnit is a unit PLUS a timeid PLUS an id
# The timeid breaks the values in the unit into different time
# periods, and the id breaks the values into different shapes

animUnit <- function(x, timeid=NULL, id=NULL) {
    if (!is.unit(x))
        stop("'x' must be a unit object")
    if (!is.null(timeid))
        timeid <- rep(timeid, length.out=length(x))
    if (!is.null(id))
        id <- rep(id, length.out=length(x))
    tu <- list(values=x, timeid=timeid, id=id)
    class(tu) <- "animUnit"
    tu
}

is.animUnit <- function(x) inherits(x, "animUnit")

as.animUnit <- function(x, ...) {
    UseMethod("as.animUnit")
}

as.animUnit.animUnit <- function(x, ...) x

as.animUnit.numeric <- function(x, unit=NULL, ...) {
    if (is.null(unit))
        stop("Require 'unit' to convert numeric vector")
    animUnit(unit(x, unit))
}

as.animUnit.unit <- function(x, ...) {
    animUnit(x)
}

# 'multVal' controls whether columns of the matrix are used as
# 'timeid' or 'id'
as.animUnit.matrix <- function(x, unit=NULL, multVal=FALSE, ...) {
    if (is.null(unit))
        stop("Require 'unit' to convert matrix")
    if (multVal) {
        animUnit(unit(x, unit), timeid=rep(1:ncol(x), each=nrow(x)))
    } else {
        animUnit(unit(x, unit), id=rep(1:ncol(x), each=nrow(x)))
    }
}

as.animUnit.list<- function(x, multVal=FALSE, ...) {
    if (!all(sapply(x, is.unit)))
        stop("All components of list must be units")
    if (multVal) {
        animUnit(do.call("unit.c", x),
                 timeid=rep(1:length(x), sapply(x, length)))
    } else {
        animUnit(do.call("unit.c", x),
                 id=rep(1:length(x), sapply(x, length)))
    }
}

listFromAnimUnit <- function(x) {
    if (is.null(x$id)) {
        if (is.null(x$timeid)) {
            n <- length(x$values)
            animUnitList <- vector("list", n)
            for (i in 1:n)
                animUnitList[[i]] <- x$values[i]
        } else {
            times <- unique(x$timeid)
            n <- length(times)
            animUnitList <- vector("list", n)
            for (i in 1:n)
                animUnitList[[i]] <- x$values[x$timeid == i]
        }
        names(animUnitList) <- paste("t", 1:n, sep="")
    } else {
        shapes <- unique(x$id)
        ns <- length(shapes)
        animUnitList <- vector("list", ns)
        for (i in 1:ns) {
            animUnitList[[i]] <-
                listFromAnimUnit(animUnit(x$values[x$id == i],
                                          x$timeid[x$id == i]))
        }
        names(animUnitList) <- paste("id", 1:ns, sep="")
    }
    animUnitList
}

print.animUnit <- function(x, ...) {
    # Generate list from animUnit and then print the list
    print(listFromAnimUnit(x))
}

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
                        duration=1, 
                        rep=FALSE, revert=FALSE,
                        begin=0) {
    animations <- list(...)
    if (is.null(animations[[1]]))
        stop("need argument to animate")
    cl <- class(grob)
    grob$animations <- c(animations,
                         list(begin=begin, duration=duration, 
                              rep=rep, revert=revert))
    class(grob) <- c("animated.grob", cl)
    grob
}
  
grid.animate <- function(path, ...) {
  grid.set(path, animateGrob(grid.get(path), ...), redraw=FALSE)
}

animate <- function(x, animation, dev) {
  UseMethod("animate")
}

# Convert to animValue then take value(s) for shape "i"
# This function is designed for animValues where timeid is NULL
# so that each time period has only ONE value
# RETURN a VECTOR
ithValue <- function(animValues, i) {
    av <- as.animValue(animValues)
    if (!is.null(av$timeid))
        stop("Expecting only one value per time point")
    if (is.null(av$id))
        av$values
    else
        av$values[av$id == i]
}

# Convert to animUnit then take unit(s) for shape "i"
# This function is designed for animUnits where timeid is NULL
# so that each time period has only ONE value
# RETURN a UNIT
ithUnit <- function(animValues, origValue, i) {
    au <- as.animUnit(animValues,
                      unit=attr(origValue, "unit"))
    if (!is.null(au$timeid))
        stop("Expecting only one value per time point")
    if (is.null(au$id))
        au$values
    else
        au$values[au$id == i]
}

# Convert to animValue then take value(s) for shape "i"
# This function is designed for animValues where there is a timeid 
# so that each time period has MULTIPLE values
# RETURN an ANIMVALUE
ithAnimValue <- function(animValues, i) {
    av <- as.animValue(animValues, multVal=TRUE)
    if (is.null(av$timeid))
        stop("Expecting multiple values per time point")
    if (is.null(av$id))
        av
    else
        animValue(av$values[av$id == i],
                  av$timeid[av$id == i])
}

# Convert to animUnit then take unit(s) for shape "i"
# This function is designed for animUnit where there is a timeid 
# so that each time period has MULTIPLE values
# RETURN an ANIMUNIT
ithAnimUnit <- function(animValues, origValue, i) {
    au <- as.animUnit(animValues,
                      unit=attr(origValue, "unit"),
                      multVal=TRUE)
    if (is.null(au$timeid))
        stop("Expecting multiple values per time point")
    if (is.null(au$id))
        au
    else
        animUnit(au$values[au$id == i],
                 au$timeid[au$id == i])
}

animate.rect <- function(x, animation, dev) {

  # We may be dealing with multiple rects that need animating
  n <- max(length(x$x), length(x$y), length(x$width), length(x$height))

  # Rep the original x/y/width/height out to be the same length
  x$x <- rep(x$x, length.out=n)
  x$y <- rep(x$y, length.out=n)
  x$width <- rep(x$width, length.out=n)
  x$height <- rep(x$height, length.out=n)
  
  # Repeating animation parameters so that each element can have
  # distinct values
  begin <- rep(x$animations$begin, length.out = n)
  dur <- rep(x$animations$duration, length.out = n)
  rep <- rep(x$animations$rep, length.out = n)
  rev <- rep(x$animations$revert, length.out = n)

  for (i in 1:n) {
    subName <- subGrobName(x$name, i)

    # If x AND y change, need to transform together
    # If width/height changes, have to animate x/y as well
    # because SVG <rect> does not have justification
    if ("x" %in% names(x$animations))
        xi <- ithUnit(x$animations$x, x$x, i)
    else
        xi <- x$x[i]
    if ("y" %in% names(x$animations))
        yi <- ithUnit(x$animations$y, x$y, i)
    else
        yi <- x$y[i]
    if ("width" %in% names(x$animations))
        widthi <- ithUnit(x$animations$width, x$width, i)
    else
        widthi <- x$width[i]
    if ("height" %in% names(x$animations))
        heighti <- ithUnit(x$animations$height, x$height, i)
    else
        heighti <- x$height[i]
    lb <- leftbottom(xi, yi, widthi, heighti, x$just, dev)
    
    switch(animation,
           x={
               svgAnimateXYWH("x", cx(lb$x, dev),
                              begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
           },
           y={
               svgAnimateXYWH("y", cy(lb$y, dev),
                              begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
           },
           width={
               # If x is also animated, this has already been handled above
               if (!("x" %in% names(x$animations))) {
                   svgAnimateXYWH("x", cx(lb$x, dev),
                                  begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
               } 
               dim <- dimToInches(ithUnit(x$animations$width, x$width, i),
                                  x$height[i], dev)
               svgAnimateXYWH("width", cw(dim$w, dev),
                              begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
           },
           height={
               if (!("y" %in% names(x$animations))) {
                   svgAnimateXYWH("y", cy(lb$y, dev),
                                  begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
               } 
               dim <- dimToInches(x$width[i],
                                  ithUnit(x$animations$height, x$height, i),
                                  dev)
               svgAnimateXYWH("height", ch(dim$h, dev),
                              begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
           },
           # Any other attribute
           {
               svgAnimate(animation,
                          paste(ithValue(x$animations[[animation]], i),
                                collapse=";"),
                          begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
           })
  }
}

animate.circle <- function(x, animation, dev) {

  # We may be dealing with multiple circles that need animating
  n <- max(length(x$x), length(x$y), length(x$r))

  # Rep the original x/y/width/height out to be the same length
  x$x <- rep(x$x, length.out=n)
  x$y <- rep(x$y, length.out=n)
  x$r <- rep(x$r, length.out=n)
  
  # Repeating animation parameters so that each element can have
  # distinct values
  begin <- rep(x$animations$begin, length.out = n)
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

    if ("x" %in% names(x$animations))
        xi <- ithUnit(x$animations$x, x$x, i)
    else
        xi <- x$x[i]
    if ("y" %in% names(x$animations))
        yi <- ithUnit(x$animations$y, x$y, i)
    else
        yi <- x$y[i]

    switch(animation,
           x={
               loc <- locToInches(xi, yi, dev)
               svgAnimateXYWH("cx", cx(loc$x, dev),
                              begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
           },
           y={
               loc <- locToInches(xi, yi, dev)
               svgAnimateXYWH("cy", cy(loc$y, dev),
                              begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
           },
           r={
               svgAnimateXYWH("r", cd(ithUnit(x$animations$r, x$r, i), dev),
                              begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
           },
           # Any other attribute
           {
               svgAnimate(animation,
                          paste(ithValue(x$animations[[animation]], i),
                                collapse=";"),
                          begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
           })
  }
}

# FIXME:  points which generate <g> elements will NOT work
#         (needs an animateTranslation as per text grobs?)
animate.points <- function(x, animation, dev) {

  # We may be dealing with multiple points that need animating
  n <- max(length(x$x), length(x$y), length(x$size))

  # Rep the original x/y/width/height out to be the same length
  x$x <- rep(x$x, length.out=n)
  x$y <- rep(x$y, length.out=n)
  x$pch <- rep(x$pch, length.out = n)
  x$size <- rep(x$size, length.out = n)

  # Repeating animation parameters so that each element can have
  # distinct values
  begin <- rep(x$animations$begin, length.out = n)
  dur <- rep(x$animations$duration, length.out = n)
  rep <- rep(x$animations$rep, length.out = n)
  rev <- rep(x$animations$revert, length.out = n)

  # Because grobs can produce multiple elements, if animation is to
  # occur on a grob it is assumed to occur on all elements, but
  # elements may simply have their properties assigned to the same
  # value multiple times.
  for (i in 1:n) {
      subName <- subGrobName(x$name, i)
      
      if ("x" %in% names(x$animations))
          xi <- ithUnit(x$animations$x, x$x, i)
      else
          xi <- x$x[i]
      if ("y" %in% names(x$animations))
          yi <- ithUnit(x$animations$y, x$y, i)
      else
          yi <- x$y[i]
      
      switch(animation,
             x={
                 loc <- locToInches(xi, yi, dev)
                 if (x$pch[i] == 1 || x$pch[i] == 16)
                     animattr <- "cx"
                 else
                     animattr <- "x"
                 svgAnimateXYWH(animattr, cx(loc$x, dev),
                                begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
             },
             y={
                 loc <- locToInches(xi, yi, dev)
                 if (x$pch[i] == 1 || x$pch[i] == 16)
                     animattr <- "cy"
                 else
                     animattr <- "y"
                 svgAnimateXYWH(animattr, cy(loc$y, dev),
                                begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
             },
             size={
                 pointsize <- cd(ithUnit(x$animations$size, x$size, i), dev)
                 if (x$pch[i] == 0) {
                     svgAnimateXYWH("width", pointsize,
                                    begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
                     svgAnimateXYWH("height", pointsize,
                                    begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
                 }
                 if (x$pch[i] == 1 || x$pch[i] == 16) {
                     svgAnimateXYWH("r", pointsize,
                                    begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
                 }
             },
             # Any other attribute
             {
                 svgAnimate(animation,
                            paste(ithValue(x$animations[[animation]], i),
                                  collapse=";"),
                            begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
             })
  }
}

# FIXME:  will produce TWO animateTranslation elements if
#         both x and y are animated ?
animate.text <- function(x, animation, dev) {
    
    # We may be dealing with multiple points that need animating
    n <- max(length(x$x), length(x$y), length(x$label))

    # Rep the original x/y/width/height out to be the same length
    x$x <- rep(x$x, length.out=n)
    x$y <- rep(x$y, length.out=n)
    
    # Repeating animation parameters so that each element can have
    # distinct values
    begin <- rep(x$animations$begin, length.out = n)
    dur <- rep(x$animations$duration, length.out = n)
    rep <- rep(x$animations$rep, length.out = n)
    rev <- rep(x$animations$revert, length.out = n)

    for (i in 1:n) {
        subName <- subGrobName(x$name, i)

        if ("x" %in% names(x$animations))
            xi <- ithUnit(x$animations$x, x$x, i)
        else
            xi <- x$x[i]
        if ("y" %in% names(x$animations))
            yi <- ithUnit(x$animations$y, x$y, i)
        else
            yi <- x$y[i]

        switch(animation,
               x={
                   loc <- locToInches(xi, yi, dev)
                   svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                                         begin[i], dur[i], rep[i], rev[i],
                                         subName, dev@dev)
               },
               y={
                   loc <- locToInches(xi, yi, dev)
                   svgAnimateTranslation(cx(loc$x, dev), cy(loc$y, dev),
                                         begin[i], dur[i], rep[i], rev[i],
                                         subName, dev@dev)
               },
               # Any other attribute
               {
                   svgAnimate(animation,
                              paste(ithValue(x$animations[[animation]], i),
                                    collapse=";"),
                              begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
               })
    }
}

doNotAnimate <- function(x, animation) {
    # Avoid doing BOTH x and y if BOTH animated
    if (all(c("x", "y") %in% x$animations) &&
        animation %in% c("x", "y") &&
        match(animation, x$animations) == max(match(c("x", "y"),
                                              x$animations)))
        TRUE
    else
        FALSE
}

animate.lines <- function(x, animation, dev) {
    if (doNotAnimate(x, animation))
        return()
    
    # NOTE:  only ever drawing ONE line
    begin <- x$animations$begin
    dur <- x$animations$duration
    rep <- x$animations$rep
    rev <- x$animations$revert
    
    subName <- subGrobName(x$name, 1)
    
    if ("x" %in% names(x$animations)) {
        au <- ithAnimUnit(x$animations$x, x$x, 1)
        xx <- au$values
        timeid <- au$timeid
    } else {
        xx <- x$x
    }
    if ("y" %in% names(x$animations)) {
        au <- ithAnimUnit(x$animations$y, x$y, 1)
        yy <- au$values
        timeid <- au$timeid
    } else {
        yy <- x$y
    }
    
    if (any(c("x", "y") %in% names(x$animations))) {
        loc <- locToInches(xx, yy, dev)
        svgAnimatePoints(cx(loc$x, dev), cy(loc$y, dev), timeid,
                         begin, dur, rep, rev, subName, dev@dev)
    }
    # Any other attribute
    if (!(animation %in% c("x", "y"))) {
        svgAnimate(animation,
                   paste(ithValue(x$animations[[animation]], 1),
                         collapse=";"),
                   begin, dur, rep, rev, subName, dev@dev)
    }
  
}
  
animate.polyline <- function(x, animation, dev) {
    if (doNotAnimate(x, animation))
        return()
    
  # If we only have one line
    if (is.null(x$id) && is.null(x$id.lengths)) {
        x$id <- rep(1L, length(x$x))
    }

  # Multiple lines exist
    if (is.null(x$id)) {
        n <- length(x$id.lengths)
        id <- rep(1L:n, x$id.lengths)
    } else {
        n <- length(unique(x$id))
        id <- x$id
    }

  # Repeating animation parameters so that each element can have
  # distinct values
    begin <- rep(x$animations$begin, length.out = n)
    dur <- rep(x$animations$duration, length.out = n)
    rep <- rep(x$animations$rep, length.out = n)
    rev <- rep(x$animations$revert, length.out = n)

    for (i in 1:n) {
        subName <- subGrobName(x$name, i)
        
        if ("x" %in% names(x$animations)) {
            au <- ithAnimUnit(x$animations$x, x$x, i)
            xx <- au$values
            timeid <- au$timeid
        } else {
            xx <- x$x[x$id == i]
        }
        if ("y" %in% names(x$animations)) {
            au <- ithAnimUnit(x$animations$y, x$y, i)
            yy <- au$values
            timeid <- au$timeid
        } else {
            yy <- x$y[x$id == i]
        }

        if (any(c("x", "y") %in% names(x$animations))) {
            loc <- locToInches(xx, yy, dev)
            svgAnimatePoints(cx(loc$x, dev), cy(loc$y, dev), timeid,
                             begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
        }
        # Any other attribute
        if (!(animation %in% c("x", "y"))) {
            svgAnimate(animation,
                       paste(ithValue(x$animations[[animation]], i),
                             collapse=";"),
                       begin[i], dur[i], rep[i], rev[i], subName, dev@dev)
        }
    }
}

# FIXME:  segments, polygons, xsplines, ...
           
primToDev.animated.grob <- function(x, dev) {
  animations <- x$animations[!names(x$animations) %in%
                             c("begin", "duration", "id", "rep", "revert")]
  for (i in names(animations)) 
      animate(x, i, dev)
  NextMethod()
}

