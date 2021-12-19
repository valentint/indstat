##  About fonts in R charts
##
##  How to use your favorite fonts in R charts:
##  https://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
##

## 1. Install 'extrafont' and load fonts

install.packages("extrafont")
library(extrafont)

## this will not work ...
font_import()

## ... use the following instead
ttf_import()


## View the available fonts
fonts()
fonttable()

##---------------------------------------------------------
## 2. Create PDF files with fonts

library(extrafont)
loadfonts()

# If you want to output to .ps files instead of .pdf, use:
# loadfonts(device="postscript")

## A. basic graphics
pdf("plot_garamond.pdf", family="Garamond", width=4, height=4.5)

plot(mtcars$mpg, mtcars$wt,
     main = "Fuel Efficiency of 32 Cars",
     xlab = "Weight (x1000 lb)",
     ylab = "Miles per Gallon")

dev.off()

## B. ggplot2
library(ggplot2)
p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
  ggtitle("Fuel Efficiency of 32 Cars") +
  xlab("Weight (x1000 lb)") + ylab("Miles per Gallon") +
  theme_bw() +
  theme(text=element_text(family="Garamond", size=14))

ggsave("ggplot_garamond.pdf", p, width=3.5, height=3.5)

##------------------------------------------------------------

## 3. Embedding fonts

# For Windows - in each session
# Adjust the path to match your installation of Ghostscript
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.05/bin/gswin32c.exe")

#  If you don't specify 'outfile', it will overwrite the original file
embed_fonts("plot_garamond.pdf", outfile="plot_garamond_embed.pdf")

embed_fonts("ggplot_garamond.pdf", outfile="ggplot_garamond_embed.pdf")
