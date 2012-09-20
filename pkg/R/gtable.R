# Here are some fixes so that ggplot2 plots work properly! ggplot2 imports
# gtable and includes some nastiness that make it difficult for gridSVG to
# parse. To support the ggplot2 package, these compatibility features have
# been created.

grobToDev.gTableChild <- function(x, dev) {
  depth <- enforceVP(x$wrapvp, dev)
  NextMethod()
  unwindVP(x$wrapvp, depth, dev)
}


grobToDev.gTableParent <- function(x, dev) {
  depth <- enforceVP(x$layoutvp, dev)
  primToDev(x, dev)
  unwindVP(x$layoutvp, depth, dev)
}

# Ripped from gtable package's grid.draw.gtable method in grid.r.
# Note that the class ordering on a "gTableChild" is switched.
gTableGrob <- function(x) {
  if (length(x$grobs) == 0) return(invisible())

  children_vps <- mapply(gtable:::child_vp,
    vp_name = gtable:::vpname(x$layout),
    t = x$layout$t, r = x$layout$r, b = x$layout$b, l = x$layout$l,
    clip = x$layout$clip,
    SIMPLIFY = FALSE)

  x$grobs <- mapply(gtable:::wrap_gtableChild, x$grobs, children_vps,
    SIMPLIFY = FALSE)

  if (inherits(x, "gTableChild")) {
    gt <- gTree(children = do.call("gList", x$grobs[order(x$layout$z)]),
      cl = c("gTableChild", "gTableParent"),
      vp = x$vp,
      wrapvp = x$wrapvp,
      layoutvp = viewport(layout = gtable:::gtable_layout(x), name = x$name))
  } else {
    gt <- gTree(children = do.call("gList", x$grobs[order(x$layout$z)]),
      cl = "gTableParent",
      vp = x$vp,
      layoutvp = viewport(layout = gtable:::gtable_layout(x), name = x$name))
  }

  gt
}

grobToDev.gtable <- function(x, dev) {
  grobToDev(gTableGrob(x), dev)
}
