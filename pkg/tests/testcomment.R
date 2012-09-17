library(gridSVG)

dev.new(width=6, height=6)
grid.rect(name = "mainrect")
grid.comment("mainrect", "This is a comment")
gridToSVG()
dev.off()
