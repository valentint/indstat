xyrect <- function(x1, y1, x2, y2, horizontal = TRUE, ...)
{
            if (horizontal)
                rect(x1, y1, x2, y2, ...)
            else rect(y1, x1, y2, x2, ...)
}

######
##  Dot plot of two variables: country and indicator
##
##  - ctvar - name of the country varable
##  - var - name of the indicator variable
##  - x - data frame containng the two variables
##
do.dotplot <- function(x, ctvar="NAME", var="AAGR", grpvar, xlab, pch, col, legend, ...)
{
    if(missing(xlab))
        xlab <- var

    if(missing(pch))
        pch <- 5

    if(missing(col))
        col <- "blue"

    pchgrp <- pch
    colgrp <- col

    x2 <- x[!is.na(x[,var]),]
    x2[,ctvar] <- reorder(x2[,ctvar], x2[,var])

    if(!missing(grpvar))
    {
        colgrp <- col[x2[,grpvar]]
        pchgrp <- pch[x2[,grpvar]]
    }

    val <- x2[,var]
    xrange <- max(val) - min(val)
    xlim <- c(min(val) - 0.1*xrange, xmax=max(val)+0.1*xrange)

    legend.list <- NULL
    if(!missing(legend))
    {
        legend.list <- list(text = list(legend, cex = .75), points = list(pch = pch, col = col, fill=col, cex = .75), space = "top", border = TRUE)
    }


    dotplot(x2[,ctvar] ~ x2[,var],
        aspect=1.5,
        xlim=xlim,
        panel = function(x, y, xmin=xlim[1]) {
            panel.xyplot(x,y, pch=pchgrp, cex=0.7, col=colgrp, fill=colgrp)
            panel.segments(rep(xmin, length(x)), y, x, y, col = "gray", lty = 2)
            panel.abline(v=1/11, col.line="black", lty=2)
            ## panel.abline(v=0, col.line="black")
            ## panel.abline(v=-50, col.line="black", lty=2)
            ## panel.abline(v=50, col.line="black", lty=2)
        },
        xlab=xlab,
        key = legend.list,
        ...)
}


do.spmap <- function(x, var, main, col=c("Custom", "Blues", "Greens", "Oranges"), pal)
{
    require(maptools)
    require(RColorBrewer)
    library(grid)
    data(wrld_simpl)

    col <- match.arg(col)
    if(col=="Custom" & missing(pal))
        stop("You have to provide either name of a color pallete in 'col' or explicit color palette in 'pal'!")

    ## substitute data for Greenland from Denmark
    ind.dk <- which(x$CT==208)
    ind.grl <- which(x$CT==304)
    if(length(ind.dk) == 1 & length(ind.grl) == 1)
    {
        x[ind.grl, var] <- x[ind.dk, var]
    }

    idx <- match(wrld_simpl$ISO3, x$ISO3)
    xOrd <- x[idx,]
    row.names(xOrd) <- wrld_simpl$ISO3
    xMap <- spCbind(wrld_simpl, xOrd)

    n <- 9
    if(is.factor(x[,var]))
      n <- length(levels(x[,var])) + 1

    if(missing(pal))
    {
        pal <- colorRampPalette(brewer.pal(n=n, col))(n)
        pal <- pal[-1]
    }

    spobj <- if(!missing(main))
                spplot(xMap[var], col.regions=pal, main=list(label=main, cex=1))
             else
                spplot(xMap[var], col.regions=pal)
    spobj$legend$right$args$key$height <- 0.3

if(FALSE)       ## try to move the legend to the bottom
{
    spobj <- spplot(xMap[var], col.regions=pal)

    key <- draw.colorkey(spobj$legend[[1]]$args$key)
    spobj$legend <- NULL # Otherwise we'd get two keys
    key$framevp$height <- unit(0.4, "npc")
    key$framevp$x <- unit(0.9, "npc")
    key$framevp$y <- unit(0.2, "npc")

    # Plot
    spobj
    grid.draw(key)
}

print(spobj)

}
