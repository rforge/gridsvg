grid.comment <- function(path, comment) {
    grid.set(path, commentGrob(grid.get(path), comment), redraw = FALSE)
}

commentGrob <- function(grob, comment) {
    grob$comment <- comment
    cl <- class(grob)
    class(grob) <- unique(c("commented.grob", cl))
    grob
}

primToDev.commented.grob <- function(x, dev) {
    svgComment(x$comment, dev@dev)
    NextMethod()
}
