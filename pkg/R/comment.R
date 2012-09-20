grid.comment <- function(name, comment, vp = NULL) {
    grid.set(name, commentGrob(name, comment, vp), redraw = FALSE)
}

commentGrob <- function(name, comment, vp = NULL) {
    ng <- nullGrob(name = name, vp = vp)
    ng$comment <- comment
    cl <- class(ng)
    class(ng) <- unique(c("comment.grob", cl))
    ng
}

primToDev.comment.grob <- function(x, dev) {
    svgComment(x$comment, dev@dev)
}
