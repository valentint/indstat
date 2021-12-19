library(yearbook2019)
library(RColorBrewer)

## create a sequential palette for usage and show colors
mypalette <- brewer.pal(7, "Oranges")
image(1:7, 1, as.matrix(1:7), col=mypalette, xlab="Oranges (sequential)", ylab="", xaxt="n", yaxt="n", bty="n")

pdf("yb-colors-cmyk.pdf", width=6, height=6, colormodel="cmyk")
showcolors(5, "cmyk")
dev.off()

pdf("yb-colors-srgb.pdf", width=6, height=6, colormodel="srgb")
showcolors(5, "srgb")
dev.off()
