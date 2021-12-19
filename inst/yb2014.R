setwd("S:/UQD/YEARBOOK/2014/R")
n<-25
plot(1:n, rep(0,n), pch=c(1:20), ylim=c(-10,10))
text(1:n, rep(1,n), labels=1:n)
##doSave(outfile="dummy", type="PDF")

#####
##  VT::06.12.2012 - YEARBOOK 2013
##
##  We use just three data sets imported from SAS:
##      MVACOD, GDPCOD and POP
##
##      MVACOD: graph.mva
##      GDPCOD: graph.gdp
##      POP:    graph.pop
##
##  #1: MVA.overall()   - MVA
##  #2: MVA.LDC()       - MVA growth trends of the least developed countries, 2000-2012
##  #3: MVA.share()     - MVA, share (in log-scale) of major country groups in world MVA
##  #4: MVA.emp()       - MVA and employment in major industrialized countries in last decade
##  #5: MVA.emerging()  - Average annual growth of MVA of selected emerging industrial economies
##  #6: MVACAP.number() - Change in the number of countries by selected range of MVA per capita


#####
##  VT::18.11.2011 - YEARBOOK 2012
##
##  We use just three data sets imported from SAS:
##      MVACOD, GDPCOD and POP
##
##      MVACOD: graph.mva
##      GDPCOD: graph.gdp
##      POP:    graph.pop
##
##  #1: MVA.overall()   - MVA
##  #2: MVA.recession() - MVA, annual growth, smooth line
##  #3: MVACAP.freq()   - MVA, POP per country (all)
##  #4: MVA.BRICS()     - MVA per country (BRICS), World; annual growth, share
##  #5: MVACAP.oil()    - MVA, POP, per country (selected)
##  #6: MVA.LDC()       - MVA, annual growth rates
##################################################################
##  VT::01.12.12010 - YEARBOOK 2011
##
##  This year the concept was changed - instead of preparing the data set
##  for each graph in SAS and producing only the graph in R, only predefined
##  datasets were prepared in SAS and each graph function selects the
##  necessary data. The data sets are:
##
##      MVACOD: graph.mva
##      GDPCOD: graph.gdp
##      POP:    graph.pop
##
##  Exception is the dta set 'graph.top' containing the data for the
##  graph Share of leading manufacturers...", i.e. top 10 leading countries
##  by share of MVA in world's total. This data set was prepared in SAS,
##  but it is preferable to move this to R. For this purpose it is
##  necessary to export from SAS a data set containing MVA for all
##  countries and country groups and for all years.
##
##  A. The ultimate solution would be to export to R a data set containing
##  the three variables MVACOD, GDPCOD and POP for all countries,
##  country groups and years. The structure could be as follows:
##
##  CT YEAR MVA GDP POP
##
##  B. Another improbement would be to export a list of country and
##  country groups names to be used in the graphs - now these are
##  hard coded in R.
##
#################################################################

###################################################
### code chunk number 2: library
###################################################
##
## Calculate Average Annual Growth Rate of X for the period [t0-tn]
##
##  r.one       - growth rate achieved in one year: last - first devided by first.
##                  We include this here, because some people use this for calculating
##                  the AAGR between several years (by deviding by the number of periods)
##  r.ave       - average of the growth rates for each year:
##                  mean(r1, r2, ..., rn)
##  r.compound  - compond annual growth rate, given as
##                  CAGR(tn,t0) = [V(tn)/V(t0)]^(1/(tn-t0)) - 1
##  r.regress   - AAGR computed by semi-log linear OLS regression
##
grate <- function(X, T, t0, tn)
{
    ipos1 <- which(T==t0)
    ipos2 <- which(T==tn)
    vn <- X[ipos2]
    v0 <- X[ipos1]
    x <- X[ipos1:ipos2]
    t <- T[ipos1:ipos2]
##    cat("\nPeriod=",T[ipos1], "-", T[ipos2],"\n")

    ## 1. "One year" growth rate
    r.one <- 100*(vn - v0)/v0/(tn-t0)

    ## 2. "Average" growth rate
    rx2 <- x[2:length(x)]
    rx1 <- x[1:length(x)-1]
    rx <- (rx2-rx1)/rx1

print(rx)

##    print(rx)
    r.ave <- 100*mean(rx)

    ## 3. Copound growth rate
    ## CAGR(tn,t0) = [V(tn)/V(t0)]^(1/(tn-t0)) - 1
    r.compound <- 100 *((vn/v0)^(1/(tn-t0)) - 1)
##    cat("\n", t0, tn, v0, vn, r2, "\n")


    ## 4. Semi-log linear OLS regression
    lx <- log(x)
    lmx <- lm(lx~t)
    r.regress <- 100*(exp(coef(lmx)[2])-1)

    ret <- list(t0=t0, tn=tn, r.one=r.one, r.ave=r.ave, r.compound=r.compound, r.regress=r.regress, lm=lmx)
    class(ret) <- "grate"
    ret
}

print.grate <- function(x)
{
    cat("\nAAGR for the period ", x$t0, "-", x$tn, "\n")
    c1 <- c("r.one", "r.ave", "r.compound", "r.regress")
    c2 <- c(x$r.one, x$r.ave, x$r.compound, x$r.regress)
    df <- cbind.data.frame(c2)
    rownames(df) <- c1
    colnames(df) <- "AAGR"
    print(round(df, 2))
}

select.mva <- function(rows, cols)
{
    if(!missing(cols) && !is.null(cols) && length(cols) > 0)
        cols <- paste("CT",cols,sep="")
    else
        cols <- NULL

    select.data(rows, cols, graph.mva)
}

select.gdp <- function(rows, cols)
{
    if(!missing(cols) && !is.null(cols) && length(cols) > 0)
        cols <- paste("CT", cols, sep="")
    else
        cols <- NULL
    select.data(rows, cols, graph.gdp)
}

select.pop <- function(rows, cols)
{
    if(!missing(cols) && !is.null(cols) && length(cols) > 0)
        cols <- paste("CT", cols, sep="")
    else
        cols <- NULL
    select.data(rows, cols, graph.pop)
}

select.data <- function(rows, cols, data)
{
    if(!missing(rows) & !is.null(rows))
        data <- data[which(rownames(data) %in% rows),, drop=FALSE]

    if(missing(cols) | is.null(cols))
        return(data)

    data <- data[, which(colnames(data) %in% cols), drop=FALSE]

    data[, cols]
}

DUMMY <- function(gname="XXX", obox=FALSE, type="pdf")
{
    plot(0,0)
    title("Not yet implemented!")
    if(obox)
        box(which="outer")
    doSave(outfile=gname, type=type)
}

## From CRAN - TeachingDemos
col2grey <- function (cols)
{
    rgb <- col2rgb(cols)
    gry <- rbind(c(0.3, 0.59, 0.11)) %*% rgb
    rgb(gry, gry, gry, maxColorValue = 255)
}


#######################
##
## Linechart: Growth of MVA per-capita by development groups at constant prices of 2000 (1990=100).
##
##  - the rownames are years, e.g. 1990-2009 - used as x-labels
##
##  - smooth lines: https://stat.ethz.ch/pipermail/r-help/2007-September/141161.html
##                  http://stackoverflow.com/questions/3480388/how-to-fit-a-smooth-curve-to-my-data-in-r
##
do.line <- function(x, obox=FALSE, type="pdf", outfile="gr-line", legend.position="topleft", ylab, smoothline=FALSE, save=FALSE)
{
    n <- nrow(x)
    p <- ncol(x)

    xlab <- rownames(x)
    tt <- as.numeric(xlab)

    leg.text <- colnames(x)
    leg.lty <- rep("solid", p)
    leg.pch=c(21,19,12,24)[1:p]
    leg.col=rep("black", p)                  #    leg.bg <- c("lightcyan", "coral", "magenta")
    leg.bg <- rep("white", p)
    plot.bg <- "white"

    dev.off()
    windows(8,5)
    par(bg=plot.bg)

    ## alter margin 4; others are default
    ##            bottom, left, top, right
    ##  defaults: 5.1, 4.1, 4.1, 2.1
    ##     empty: 3.1, 3.1, 2.1, 2.1
    mar.bottom <- 3.1
    mar.left   <- 4.1
    if(missing(ylab))
    {
        ylab <- ""
        mar.left <- 3.1
    }
    oldpar <- par(mar = c(mar.bottom, mar.left, 2.1, 2.1))

    xlim <- c(min(tt),max(tt))
    ylim <- c(min(x), max(x))
    ylim[1] <- ylim[1] - 0.1*abs(ylim[1])
    ylim[2] <- ylim[2] + 0.1*abs(ylim[2])

    print(xlab)

    plot(tt, x[,1], cex.lab=1.2, xlab="", ylab=ylab,
                xlim=xlim, ylim=ylim, type="n",
                axes=FALSE)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col="white", border="black")
    if(obox)
        box(which="outer")

    axis(side=1, at=pretty(tt, n=min(n, 14)), las=2)

    xaxis <- pretty(range(x), n=6)
    axis(side=2, at=xaxis, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    ind <- 1
    for(ind in 1:p)
    {
        yy <- list(x=tt, y=x[, ind])
        if(smoothline)
        {
            yy <-predict(interpSpline(tt, x[,ind]))
        }
        lines(yy$x, yy$y, lty=leg.lty[ind], col=leg.col[ind])
        points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)
    }

    abline(h=0)

    legend(legend.position, legend=leg.text, lty=leg.lty, pch=leg.pch, col=leg.col, bg="white")

    if(save)
        doSave(outfile=outfile, type=type)
}

## Barchart: Industrialized-Developing
do.bar.2013 <- function(x, obox=FALSE, type="pdf", outfile="gr-bar", ylim, ylab, legend.position="topright",
    xaxis.nsmall,
    src.text, src.at=2, src.line=2,
    log="",
    pattern=TRUE)
{
    plot.bg <- "white"

    n <- nrow(x)
    x <- as.matrix(x)

    dev.off()
    windows(8,5)
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))
    par(bg=plot.bg)

    density <- c(25, 100, 25)[1:n]
    angle <- c(45, 0, 135)[1:n]
    col=gray.colors(n)

    if(missing(ylim))
    {
        ylim <- c(0, max(x))
        ylim[2] <- ylim[2] + 0.1*ylim[2]
    }

print(ylim)
    bb <- if(pattern)
    {
        barplot(x, ylim=ylim, beside=TRUE, axes=FALSE, border="black", col=col, density=density, angle=angle, log=log)
    } else {
        barplot(x, ylim=ylim, beside=TRUE, axes=FALSE, border="black", log=log, col=col)
    }

    par(mgp = c(3,0.5,0))
    ylab1 <- pretty(c(0,ylim[2]), n=6)
    labels <- ylab1
    if(!missing(xaxis.nsmall))
        labels <- format(ylab1, nsmall=xaxis.nsmall)

    ylab1 <- c(0.1, 1, 10, 100)
    labels <- format(ylab1)
    axis(side=2, at=ylab1, labels=labels, lty="dotted", lwd=0.5, tck=TRUE, las=1)
    if(!missing(ylab))
        mtext(ylab, side = 2, line=2, , cex=1)      # "in % of world total"
    par(mgp = c(3,2,0))

    abline(h=0)

    bb <- if(pattern)
    {
        legend(legend.position, legend=rownames(x), density=density, angle=angle)
    } else {
        legend(legend.position, legend=rownames(x), fill=col)
    }

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4])
    if(obox)
        box(which="outer")

    box()

##  Source text
    if(!missing(src.text))
        mtext(text=src.text, side=1, at=src.at, line=src.line)

    doSave(outfile=outfile, type=type)
    par(oldpar)
}

## Barchart: Industrialized-Developing
do.bar <- function(x, obox=FALSE, type="pdf", outfile="gr-bar", ylim, ylab, xlab, legend.position="topright")
{
    n <- nrow(x)
    x <- as.matrix(x)

    dev.off()
    windows(8,5)
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))
    par(bg=plot.bg)

    density <- c(25, 100)
    angle <- c(45, 0)

    if(missing(ylim))
    {
        ylim <- c(0, max(x))
        ylim[2] <- ylim[2] + 0.1*ylim[2]
    }

    col <- c("black", "lightgray")
print(ylim)

    bb <- if(pattern) {
        barplot(x[1:2,], ylim=ylim, beside=TRUE, axes=FALSE, border="black", col=col, density=density, angle=angle)
    } else {
        barplot(x[1:2,], ylim=c(0,100), beside=TRUE, axes=FALSE, border="black")
    }

    par(mgp = c(3,0.5,0))
    ylab1 <- pretty(c(0,ylim[2]), n=6)
    axis(side=2, at=ylab1, ylab=ylab1, lty="dotted", lwd=0.5, tck=TRUE, las=1)
    if(!missing(ylab))
        mtext(ylab, side = 2, line=2, , cex=1)      # "in % of world total"
    if(!missing(xlab))
        mtext(xlab, side = 1, line=2, , cex=1)      # "in % of world total"
    par(mgp = c(3,2,0))

    abline(h=0)

    if(pattern)
    {
        legend(legend.position, legend=rownames(x), density=density, angle=angle, fill=col)
    } else
    {
        legend(legend.position, legend=rownames(x), fill=col)
    }

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4])
    if(obox)
        box(which="outer")

    doSave(outfile=outfile, type=type)
    par(oldpar)
}

## Barchart: Industrialized-Developing
do.bar.line <- function(x1, x2, obox=FALSE, type="pdf", outfile="gr-bar", ylim, ylab, legend.position="topleft")
{
    dev.off()
    windows(8,5)
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))
    par(bg=plot.bg)

    density <- c(25)
    angle <- c(45)
    bar.col <- c("lightgray")
    x1 <- as.matrix(x1)
    x2 <- as.matrix(x2)

    if(missing(ylim))
    {
        ylim <- c(min(x1, x2), max(x1, x2))
        ylim[1] <- ylim[1] - 0.1*abs(ylim[1])
        ylim[1] <- min(0, ylim[1])
        ylim[2] <- ylim[2] + 0.1*abs(ylim[2])
    }

    ## no axis, no names below each bar
    if(pattern) {
        bb <- barplot(x2, ylim=ylim, beside=TRUE, names.arg = c(), axisnames=FALSE, axes=FALSE, border="black", col=bar.col, density=density, angle=angle)
    } else {
        bb <- barplot(x2, ylim=ylim, beside=TRUE, names.arg = c(), axes=FALSE, col="lightgray", border="black")
    }

    par(mgp = c(3,0.5,0))
    ylab1 <- pretty(c(ylim[1],ylim[2]), n=6)
    axis(side=2, at=ylab1, ylab=ylab1, lty="dotted", lwd=0.5, tck=TRUE, las=1)
    if(!missing(ylab))
        mtext(ylab, side = 2, line=2, , cex=1)      # "in % of world total"

    n <- nrow(x1)
    n <- nrow(x1)
    p <- ncol(x1)
    xlab <- rownames(x1)
    tt <- as.numeric(xlab)
    xlim <- c(min(tt),max(tt))

##    usr <- par("usr")
##    rect(usr[1], usr[3], usr[2], usr[4], col="white", border="black")
    if(obox)
        box(which="outer")

    axis(side=1, at=bb, labels=pretty(tt, n=min(n, 14)), las=1)

##    xaxis <- pretty(range(x), n=6)
##    axis(side=2, at=xaxis, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    leg.text <- colnames(x1)
    leg.lty <- rep("solid", p)
    leg.pch=c(12,19,21,24)[1:p]
    leg.col=c("black", "white")
    leg.bg <- rep("white", p)
    plot.bg <- "white"

    ind <- 1
    for(ind in 1:p)
    {
        lines(bb, x1[,ind], lty=leg.lty[ind], col=leg.col[ind])
        points(bb, x1[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)
    }


    leg.text <- c("Annual MVA growth rates", "Share in the world total MVA")
    ##legend(legend.position, legend=leg.text, lty=leg.lty, pch=leg.pch, col=leg.col, bg="white")
##    legend(legend.position, legend=leg.text, lty=leg.lty, bg="white")
    leg <- legend(legend.position, legend=leg.text, lty=leg.lty, col=leg.col, pch=leg.pch, bg="white")

    rx0 <- leg$text$x[2]-0.75
    ry0 <- leg$text$y[2]-0.5
    rect(rx0, ry0, rx0+0.5, ry0+0.75, density=density, angle=angle, col=bar.col, border="black")

    par(mgp = c(3,2,0))

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4])
    if(obox)
        box(which="outer")

    doSave(outfile=outfile, type=type)
    par(oldpar)
}

doSave <- function(outfile, type="ignore")
{
    savePlot(file=outfile, type="pdf")
    savePlot(file=paste("1",outfile,sep=""), type="pdf")
    savePlot(file=outfile, type="ps")
    savePlot(file=paste("1",outfile,sep=""), type="ps")

    pdffile <- paste(outfile, ".pdf", sep="")
    psfile <- paste(outfile, ".ps", sep="")
    tmpps <- "xxx.ps"
    cmd <- paste("pdftops -paper match", pdffile, tmpps)
    system(cmd)

    embedFonts(file=tmpps, outfile=psfile, options="-dUseCIEColor=true -dPDFSETTINGS=/prepress")

    cmd <- paste("S:/UQD/YEARBOOK/2014/R/ps2pdf",psfile, pdffile)
    system(cmd)
    cat("\nFile with embedded fonts saved: ",cmd,"\n")
}


#####################################################################################################################
##
##  VT::18.11.2013
##
##  Line chart
##  Annual MVA growth - Industrialized, Developing, World
##  2005-2013
##  Smoothed lines
##
MVA.growth <- function(obox=FALSE, type="pdf", years=2004:2013, windows=FALSE, save=FALSE)
{
    if(windows)
        windows(8,5)

    ## Select and prepare the data
    cgrps <- c("IND", "DEV", "WOR")
    cnames <- c("Industrialized economies", "Developing and EIE", "World")

    x1 <- x <- select.mva(years, cgrps)       # select countries and years

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    x[,] <- 0
    for(i in 1:p)
        for(j in 2:n)
            x[j,i] <- 100*x1[j,i]/x1[j-1,i] - 100

    rownames(x) <- years
    colnames(x) <- cnames
    x <- x[-1,]

    print(round(x, 2))
    do.line(x, outfile="MVA_growth", ylab="Annual growth in % ", legend.position="bottomright", smoothline=TRUE)
    xtable(x)
}

####################################
##
##  VT::18.11.2013
##
##  Line chart
##  MVA and GDP growth of developing and emerging industrial economies - (2000=100)
##
MVAGDP.growth <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 2000:2013
    cgrps <- c("DEV")
    cnames <- c("Developing and EIE")

    mva <- select.mva(years, cgrps)       # select countries and years
    gdp <- select.gdp(years, cgrps)       # select countries and years

    x <- cbind.data.frame(MVA=mva, GDP=gdp)
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
print(x)
    for(i in 1:p)
        x[,i] <- 100*x[,i]/x[1,i]

    rownames(x) <- years
    print(round(x, 2))

    do.line(x, outfile="MVAGDP_growth", ylab="Growth in % (2000=100)")
}

####################################
##
##  VT::18.11.2013
##
##  Pie chart
##  MVA.distribution: Distribution of the world MVA, 2013
##
MVA.distr <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 2013
    cgrps <- c("916", "EIEX", "DEVOT", "LDC", "IND")
    cnames <- c("China", "Emerging Industrial Economies", "Other developing economies", "Least developed countries", "Industrialized Economies")

    mva <- select.mva(years, cgrps)       # select countries and years

    x <- as.matrix(mva)
    n <- nrow(x)
    p <- ncol(x)

    rownames(x) <- years
    colnames(x) <- cnames
    names(x) <- cnames
    print(round(x, 2))

    ## do.line(x, outfile="MVAGDP_growth", ylab="Growth in % (2000=100)")
    pie(x)
}

####################################
##
##  VT::6.12.2012
##
##  Line chart
##  MVA growth - industrialized, developing, world (1995=100)
##
MVA.overall.2013 <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 2000:2012
    cgrps <- c("WOR", "IND", "EIE", "DEVOT")
    cnames <- c("World", "Industrialized economies", "EIEs incl.China", "Other developing economies")

    x <- select.mva(years, cgrps)       # select countries and years

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    for(i in 1:p)
        x[,i] <- 100*x[,i]/x[1,i]

    rownames(x) <- years
    colnames(x) <- cnames

    do.line(x, outfile="MVA_overall", ylab="Growth in % (2000=100)")
}

#####################################################################################################################
##
##  VT::6.12.2012
##
##  Line chart
##  MVA growth - industrialized, developing, world (1995=100)
##
MVA.LDC.2013 <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 2000:2012
    cgrps <- c("LDCAF", "LDCAS", "LDCSA", "LDC")
    cnames <- c("African LDCs", "Asian LDCs", "Small island LDCs", "All LDCs")

    x <- select.mva(years, cgrps)       # select countries and years

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    for(i in 1:p)
        x[,i] <- 100*x[,i]/x[1,i]

    rownames(x) <- years
    colnames(x) <- cnames
    do.line(x, outfile="MVA_LDC", ylab="Growth in % (2000=100)")
}

#####################################################################################################################
##
##  VT::6.12.2012
##
##  Bar chart
##  Share of MVA of groups:
##  Industrialized, EIEs, Developing and LDC
##
MVA.share <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- c(2000, 2005, 2012)
    cgrps <- c("IND", "EIE", "DEVOT", "LDC", "WOR")
    cnames <- c("Industrialized", "EIEs incl. China", "Other developing", "LDCs")

    xmva <- select.mva(years, cgrps)       # select countries and years
    world <- xmva[,ncol(xmva)]
    xmva <- as.matrix(xmva[,-ncol(xmva)])

    ## check that world total os OK :)
    print(all.equal(rowSums(xmva), world, check.attributes=FALSE))

    n <- nrow(xmva)
    p <- ncol(xmva)

    x <- 100 * xmva/world

    rownames(x) <- years
    colnames(x) <- cnames
##    do.bar.2013(x, outfile="MVA_share", ylim=c(0.1, 100), ylab="Share in percent", xaxis.nsmall=1, src.text="Source: UNIDO Database", src.at=2, src.line=2)
    do.bar.2013(x, outfile="MVA_share", ylim=c(0.1, 100), ylab="Share in percent (log scale)", log="y", pattern=FALSE)
}

#####################################################################################################################
##
##  VT::18.11.2011
##
##
MVA.emp <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 2001:2010
    emp <- c(-2.5, -4.2, -3.4, -3.4, -2.5, -1.5, 1.2, -3.1, -5.2, 2.9)
    mva <- c(-3.4, 0.7, 2.8, 5.9, 3.4, 3.2, 3.8, -3.4, -13.5, 4.4)

    cnames <- c("Employment", "MVA")

    x <- cbind(emp, mva)
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    rownames(x) <- years
    colnames(x) <- cnames
    print(round(x,2))

    do.line(x, outfile="MVA_emp", legend.position="bottomleft", smoothline=TRUE, ylab="Annual growth rate in %")
}


#############################################################################################################
##
##  VT::6.12.2012
##
##  Bar chart
##  Average annual growthrate of MVA:
##  Selected countries
##
MVA.emerging <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 2001:2012
    year1 <- years[2]
    year2 <- 2007
    year3 <- years[length(years)]
    cgrps <- c("156", "356", "710", "076", "484")
    cnames <- c("China", "India", "South Africa", "Brazil", "Mexico")

    xmva <- select.mva(years, cgrps)       # select countries and years
    xmva <- as.matrix(xmva)

    n <- nrow(xmva)
    p <- ncol(xmva)
    x <- matrix(0, ncol=length(cgrps), nrow=2)
    for(i in 1:p)
    {
        x[1, i] <- grate(xmva[,i], years, year1-1, year2)$r.ave
        x[2, i] <- grate(xmva[,i], years, year2-1, year3)$r.ave
    }

    rownames(x) <- c(paste(year1, "-", year2, sep=""), paste(year2, "-", year3, sep=""))
    colnames(x) <- cnames
    do.bar(x, outfile="MVA_emerging", ylab="Average annual growth rate in percent")
}

#############################################################################################################
##
##  VT::6.12.2012
##
##  Bar chart
##  Frequence table of MVA per capita
##  1992 and 2012
##
MVA.numbers <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    year1 <- 1992
    year2 <- 2012

    m.rcodes <- rcodes[which(rcodes$COMPTYPE == "M"),]
    cgrps <- m.rcodes$ACODE
    cols <- paste("CT", cgrps, sep="")

    cmva <- colnames(graph.mva)
    ct <- m.rcodes[which(cols %in% cmva),]
    cgrps <- cgrps[which(cols %in% cmva)]

    xmva1 <- select.mva(year1, cgrps)           # select countries and years
    xmva2 <- select.mva(year2, cgrps)           # select countries and years
    xmva2x <- select.mva(year2-1, cgrps)        # select countries and years
    xmva2y <- select.mva(year2-2, cgrps)        # select countries and years
    xmiss2 <- c(which(is.na(xmva2)))

    print(names(xmva2[xmiss2]))

    ## if missing take it from the previous year
    xmva2[xmiss2] <- xmva2x[xmiss2]
    xmiss2 <- c(which(is.na(xmva2)))
    print(names(xmva2[xmiss2]))

    ## if still missing take it from two years ago
    xmva2[xmiss2] <- xmva2y[xmiss2]
    xmiss2 <- c(which(is.na(xmva2)))
    print(names(xmva2[xmiss2]))

    xpop1 <- select.pop(year1, cgrps)       # select countries and years
    xpop2 <- select.pop(year2, cgrps)       # select countries and years

    xmiss <- unique(c(which(is.na(xmva1)), which(is.na(xpop1)), which(is.na(xmva2)), which(is.na(xpop2))))

    xmva1 <- xmva1[-xmiss]
    xmva2 <- xmva2[-xmiss]
    xpop1 <- xpop1[-xmiss]
    xpop2 <- xpop2[-xmiss]

    x1 <- as.numeric(xmva1/xpop1)
    x2 <- as.numeric(xmva2/xpop2)

    breaks <- c(-Inf, 100, 1000, 5000, Inf)
    labels <- c("Less than 100","100-1000","1000-5000", "More than 5000")
    xx1 <- cut(x1, breaks=breaks, labels=labels)
    xx2 <- cut(x2, breaks=breaks, labels=labels)

    x <- rbind(table(xx1), table(xx2))

    rownames(x) <- c(year1, year2)
    colnames(x) <- labels
    do.bar(x, outfile="MVACAP_numbers", ylab="Number of countries", xlab="MVA per capita at constant 2005 prices")
}

#####################################################################################################################
##
##  VT::18.11.2011
##
##  Line chart
##  MVA growth - industrialized, developing, world (1995=100)
##
MVA.overall <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 1995:2011
    cgrps <- c(933, 903, 918)
    cnames <- c("Industrialized countries", "Developing countries", "World")

    x <- select.mva(years, cgrps)       # select countries and years

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    for(i in 1:p)
        x[,i] <- 100*x[,i]/x[1,i]

    rownames(x) <- years
    colnames(x) <- cnames
    do.line(x, outfile="MVA_overall", ylab="Growth in % (1995=100)")
}

#####################################################################################################################
##
##  VT::18.11.2011
##
##
MVA.recession <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 2005:2011
    cgrps <- c(933, 903, 918)
    cnames <- c("Industrialized countries", "Developing countries", "World")

    x <- select.mva(years, cgrps)       # select countries and years

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    x1 <- x
    x1[1,] <- 0
    for(i in 1:p)
    {
        for(j in 2:n)
            x1[j,i] <- 100*x[j,i]/x[j-1,i] - 100
    }
    rownames(x1) <- years
    colnames(x1) <- cnames
    x1 <- x1[-1,]
    print(round(x1,2))

    do.line(x1, outfile="MVA_recession", legend.position="bottomleft", smoothline=TRUE, ylab="Annual growth rate in %")
}


#####################################################################################################################
##
##  VT::18.11.2011
##
##  Line chart
##  MVA growth rates - LDC, developing (1999=0)
##
MVA.LDC <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 2000:2011
    cgrps <- c(917, 899)
    cnames <- c("LDCs", "Developing countries")

    x <- select.mva(years, cgrps)       # select countries and years

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

print(round(x))
    x1 <- x
    x1[1,] <- 0
    for(i in 1:p)
    {
        for(j in 2:n)
            x1[j,i] <- 100*x[j,i]/x[j-1,i] - 100
    }
    x2 <- x[,1]/x[,2]*100
    x1 <- x1[-1,]
    x2 <- x2[-1]
print(round(x1,1))
print(round(x2,1))

    rownames(x1) <- years[-1]
    colnames(x1) <- cnames
    do.bar(t(x1), outfile="MVA_LDC", ylim=c(-2.5, 11), ylab="Annual growth rate in %")
}

#####################################################################################################################
##
##  VT::24.11.2011
##
##  Histogram
##  MVA per capita - all countries
##
MVACAP.freq <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 2011
    cgrps <- NULL
##    cnames <- c()

    x <- select.mva(years, cgrps)       # select countries and years
    x.mva <- x[,which(colnames(x) <= "CT895")]

    x <- select.pop(years, cgrps)       # select countries and years
    x.pop <- x[,which(colnames(x) <= "CT895")]

    stopifnot(dim(x.mva) == dim(x.pop))
    stopifnot(all.equal(colnames(x.mva), colnames(x.pop)))

    n <- nrow(x.mva)
    p <- ncol(x.mva)
    x.mva <- as.matrix(x.mva)
    x.pop <- as.matrix(x.pop)

    x <- x.mva/x.pop
    x <- x[,-which(is.na(x)), drop=FALSE]
    y <- sqrt(x[1,])

    dev.off()
    windows(8,5)
    par(bg=plot.bg)
##    histogram(y, type="count", breaks=c(0, 2.5, 5, 10, 20, 40, 50, 60, 70, 80, 90, 100), main="", xlab="MVA per capita, squared")
    hist(y, breaks=c(0, 2.5, 5, 10, 20, 40, 50, 60, 70, 80, 90, 100), main="", xlab="MVA per capita, squared", col="white")
    lines(density(y), lwd=1.7)
    box(which="plot")
    doSave(outfile="MVACAP_freq", type=type)
}


#####################################################################################################################
##
##  VT::18.11.2011
##
##  Line chart
##  MVA growth rates - LDC, developing (1999=0)
##
MVA.BRICS <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 2001:2011
    cgrps <- c("076", 643, 356, 156, 710)
#    cnames <- c("Brazil", "Russian Federation", "India", "China", "South Africa")

    x <- select.mva(years, cgrps)       # select countries and years
    n <- nrow(x)
    p <- ncol(x)
    x <- as.matrix(x)

print(round(x))
    x.brics <- rowSums(x)
    x.world <- select.mva(years, c(918))

    x1 <- x.brics
    x1[1] <- 0
    for(j in 2:n)
        x1[j] <- 100*x.brics[j]/x.brics[j-1] - 100
    x2 <- x.brics/x.world*100
    x1 <- as.matrix(x1[-1])
    x2 <- as.matrix(x2[-1])

    rownames(x1) <- rownames(x2) <- years[-1]
    colnames(x1) <- c("Annual MVA growth rates of BRICS")
    colnames(x2) <- c("BRICS share in world MVA")

print(round(x1,1))
print(round(x2,1))

    do.bar.line(x1, x2, outfile="MVA_BRICS", legend="topleft", ylab="in percent")
}

#####################################################################################################################
##
##  VT::24.11.2011
##
##  Vertical bar plot, 2001 and 2011
##  MVA per capita - selected countries
##
MVACAP.oil <- function(obox=FALSE, type="pdf")
{

    gname <- "MVACAP_oil"

    ## Select and prepare the data
##    784=United Arab Emirates
##    634=Qatar
##    414=Kuwait
##    682=Saudi Arabia
##    862=Venezuela
##    512=Oman
##    364=Iran
##    360=Indonesia
##    218=Ecuador
##    434=Libiya
##    "012"=Algeria
##    "024"=Angola
##   736=Sudan
##    566=Nigeria
##    368=Iraq

    years <- c(2001, 2011)
    cgrps <- c(784, 634, 414, 682, 862, 512, 364, 360, 218, 434, "012", "024", 736, 566, 368)
    cnames <- rcodes[which(rcodes$ACODE %in% cgrps),-c(2,4)]

    x.mva <- select.mva(years, cgrps)       # select countries and years
    x.pop <- select.pop(years, cgrps)       # select countries and years

    stopifnot(dim(x.mva) == dim(x.pop))
    stopifnot(all.equal(colnames(x.mva), colnames(x.pop)))

    x.mva <- as.matrix(x.mva)
    x.pop <- as.matrix(x.pop)

    xx <- x.mva/x.pop
print(    colnames(xx))
print(paste("CT", cnames$ACODE, sep=""))

    imatch <- match(colnames(xx), paste("CT", cnames$ACODE, sep=""))
    colnames(xx) <- cnames[imatch,]$DESC

    ina <- unique(c(which(is.na(xx[1,])),  which(is.na(xx[2,]))))
    xx <- xx[,-ina]
    iord <- order(xx[2,])
    x <- xx[c(2,1),iord]
    n <- nrow(x)
    p <- ncol(x)
print(t(x))

    dev.off()
    windows(8,5)

    ## alter margin 4; others are default
    oldpar <- par(mar = c(1.1, 14, 3.1, 1.1))
    par(las=2)

    xlim<-c(4.5, max(x)+0.1*max(x))

    density <- c(30, 100)
    angle <- c(45, 90)
    space <- c(0,0.5)
    col <- c(col2grey(3), col2grey(7))
    cex.axis <- 0.9

    ## Pattern not possible if log scale used.
    bb <- if(pattern) {
##         barplot(x, width=rep(4,2*n), log="x", space=space, beside=TRUE, axes=FALSE, horiz=TRUE, border="black", xlim=xlim, col=col, density=density, angle=angle)
        barplot(x, width=rep(4,2*n), log="x", space=space, beside=TRUE, axes=FALSE, horiz=TRUE, border="black", xlim=xlim, col=col, angle=c(0,45))
    } else {
        barplot(x, width=rep(4,2*n), log="x", space=space, beside=TRUE, axes=FALSE, horiz=TRUE, border="black")
    }

    mtext("MVA per capita (log scale)", las=0, line=1.5)

##    xaxis.lab <- pretty(exp(xlim), n=6)
##    xaxis <- log(xaxis.lab)
    par(mgp = c(3,0.5,0))
    axis(side=3, at=c(1, 10,100, 1000, 10000), lty="dotted", lwd=0.5, tck=TRUE, las=1, cex.axis=cex.axis)
    par(mgp = c(3,2,0))
    if(pattern) {
##        legend("bottomright", legend=rownames(x)[n:1], fill=col[n:1], density=density[n:1], angle=angle[n:1], bg="white")
        legend("bottomright", legend=rownames(x)[n:1], fill=col[n:1], bg="white")
    } else {
        legend("bottomright", legend=rownames(x)[n:1], fill=grey.colors(p)[n:1], bg="white")
    }

##  Here can be added text on the top horizontal axis
##  mtext("US$ at 2000 constant prices", side = 3, line=2, , cex=cex.axis, las=1)

    usr <- par("usr")
    ##rect(usr[1], usr[3], usr[2], usr[4], col="red")
    rect(10^usr[1], usr[3], 10^usr[2], usr[4])
    if(obox)
        box(which="inner")

    doSave(outfile=gname, type=type)
    par(oldpar)
}


#########################################################################################
##
##  VT::01.12.2010
##
##  Line chart
##  MVA growth - regions in Europe (1995=100)
##
MVA.europe.2011 <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 1995:2010
    cgrps <- c(960, 898, 897, 961)
    cnames <- c("CIS", "EU-12", "EU-15", "Other Europe")

    x <- select.mva(years, cgrps)       # select countries and years

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    for(i in 1:p)
        x[,i] <- 100*x[,i]/x[1,i]

    rownames(x) <- years
    colnames(x) <- cnames
    do.line(x, outfile="MVA_europe")
}

##
##  VT::01.12.2010
##
##  Barchart (stacked bars)
##  Distribution of total MVA of developing countries.
##
MVA.distr.2011 <- function(obox=FALSE, type="pdf")
{
    gname <- "MVA_distr"

    ## Select and prepare the data
    years <- c(1990, 1995, 2000, 2005, 2010)
    cgrps <- c(916, 915, 990, 917)
    cnames <- c("China", "NICs", "Other developing \ncountries", "LDCs")

    x <- select.mva(years, cgrps)       # select countries and years
    xdev <- select.mva(years, 903)      # select developing countries
    xdev <- as.matrix(xdev) %*% rep(1,4)           #
    x <- round(x/xdev*100, 2)                      # calculate shares in developing countries
    x <- t(x)
    rownames(x) <- cnames

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    dev.off()
    windows(8,5)

    ## alter margins
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))
    par(bg=plot.bg)

    width <- 0.5
    ind <- 1:4
    density <- c(25, 100, 25, 100)
    angle <- c(45, 0, 135, 0)
    col <- grey.colors(n+1)[2:(n+1)]
    col.pattern <- c("black", col[1], "black", col[4])

    if(pattern) {
        bp <-  barplot(x[ind,], ylim=c(0,100), width=width, space=2, beside=FALSE, axes=FALSE, border="black", col=col.pattern, density=density, angle=angle)
    } else {
        bp <-  barplot(x[ind,], ylim=c(0,100), width=width, space=2, beside=FALSE, axes=FALSE, border="black", col=col)
    }

    ylab1 <- pretty(c(0,110), n=8)
    axis(side=2, at=ylab1, ylab=ylab1, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    bp1 <- bp - width/2
    bp2 <- bp + width/2
    xx <- x[ind,]
    x0 <- rep(0,p)

    for(i in 1:(n-1))
    {
        x0 <- x0 + xx[i,]
        for(j in 1:(p-1))
            lines(c(bp2[j],bp1[j+1]), x0[j:(j+1)])
    }


    if(pattern) {
        legend("bottomright", legend=rownames(x[ind,])[n:1], bg="white", fill=col.pattern[n:1], density=density[n:1], angle=angle[n:1])
    } else {
        legend("bottomright", legend=rownames(x[ind,])[n:1], fill=col[n:1], bg="white")
    }

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4])
    if(obox)
        box(which="outer")

    doSave(outfile=gname, type=type)
    par(oldpar)
}


##
##  VT::01.12.2010
##
##  Line chart
## GDP and MVA growth of developing countries.
##
GDPMVA.dev.2011 <- function(obox=FALSE, type="pdf")
{

    ## Select and prepare the data
    years <- 1990:2010
    cgrps <- c(903)

    GDP <- select.gdp(years, cgrps)       # select countries and years
    MVA <- select.mva(years, cgrps)       # select countries and years
    x <- cbind.data.frame(GDP, MVA)
    rownames(x) <- years

    n <- nrow(x)
    p <- ncol(x)

    for(i in 1:p)
        x[,i] <- 100*x[,i]/x[1,i]

    tt <- xlab <- years

    leg.text <- colnames(x)
    leg.lty <- c("solid", "solid")
    leg.pch=c(19,21)
    leg.col=c("black","black")
##    leg.bg <- c("lightcyan", "coral")
    leg.bg <- c("white", "white")
    plot.bg <- "white"


    dev.off()
    windows(8,5)
    par(bg=plot.bg)

    ## alter margin 4; others are default
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))

    xlim <- c(min(tt),max(tt))
    ylim <- c(min(x), max(x))
    ylim[1] <- ylim[1] - 0.1*ylim[1]
    ylim[2] <- ylim[2] + 0.1*ylim[2]

    plot(tt, x[,1], xlab="", cex.lab=1.2, ylab="",
                xlim=xlim, ylim=ylim, type="n",
                axes=FALSE)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col="white", border="black")
    if(obox)
        box(which="outer")

##  R does not accept rotation of the lables by e.g. srt=90
    axis(side=1, at=tt, lab=xlab, las=2)

    ylab1 <- pretty(range(x), n=8)
    axis(side=2, at=ylab1, ylab=ylab1, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    ind <- 1
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    ind <- 2
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    legend("topleft", legend=leg.text, lty=leg.lty, pch=leg.pch, col=leg.col, bg="white")
    doSave(outfile="GDPMVA_dev", type=type)
}

##
##  VT::01.12.2010
##
##  Bar chart chart
##  Share of MVA in GDP: Industrialized and Developing groups, 2000 and 2010
MVAGDP.share.2011 <- function(obox=FALSE, type="pdf")
{

    ## Select and prepare the data
    years <- c(2000, 2010)
    cgrps <- c(933, 903, 916, 915, 917, 990)
    cnames <- c("Industrialized\ncountries", "Developing\ncountries", "China", "NICs", "LDCs", "Other developing\ncountries")

    GDP <- select.gdp(years, cgrps)       # select countries and years
    MVA <- select.mva(years, cgrps)       # select countries and years

    x <- 100*MVA/GDP
    colnames(x) <- cnames

    n <- nrow(x)
    p <- ncol(x)

    x <- as.matrix(x)

    dev.off()
    windows(8,5)
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))
    par(bg=plot.bg)

    density <- c(25, 100)
    angle <- c(45, 0)
    col <- c("black", "lightgray")

    ylim <- c(0, round(max(x) + 5))

    if(pattern) {
        barplot(x[1:2,], ylim=ylim, beside=TRUE, axes=FALSE, border="black", col=col, density=density, angle=angle)
    } else {
        barplot(x[1:2,], ylim=ylim, beside=TRUE, axes=FALSE, border="black")
    }

    par(mgp = c(3,0.5,0))
    ylab1 <- pretty(c(0,ylim[2]), n=6)
    axis(side=2, at=ylab1, ylab=ylab1, lty="dotted", lwd=0.5, tck=TRUE, las=1)
#
#   Add here some text on the Y-axis
#   mtext("in % of world total", side = 2, line=2, , cex=1)
    par(mgp = c(3,2,0))

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4])

    if(pattern) {
        legend("topright", legend=rownames(x), bg="white", fill=col[n:1], density=density[n:1], angle=angle[n:1])
    } else {
        legend("topright", legend=rownames(x), fill=col[n:1], bg="white")
    }

    if(obox)
        box(which="outer")

    doSave(outfile="MVAGDP_share", type=type)
    par(oldpar)
}

## VT::02.12.2010
## Horizontal Barchart:
##  2000-2010, 10 countries, by share of MVA in world's total
##
MVA.top10.2011 <- function(obox=FALSE, type="pdf")
{
    gname <- "MVA_TOP10"

    x <- graph.top

    n <- nrow(x)
    p <- ncol(x)
    x <- t(x)

    dev.off()
    windows(8,5)

    ## alter margin 4; others are default
    oldpar <- par(mar = c(1.1, 8.5, 3.1, 1.1))
    par(las=2)

    xlim<-c(0, max(x)+0.20*max(x))

    density <- c(30, 100)
    angle <- c(45, 90)
    space <- c(0,0.5)
    col <- c("black", "lightgray")
    cex.axis <- 0.9

    if(pattern) {
        barplot(x, width=rep(4,2*n), space=space, beside=TRUE, axes=FALSE, horiz=TRUE, border="black", xlim=xlim, col=col, density=density, angle=angle)
    } else {
        barplot(x, width=rep(4,2*n), space=space, beside=TRUE, axes=FALSE, horiz=TRUE, border="black", xlim=xlim)
    }

    xaxis <- pretty(xlim, n=6)
    par(mgp = c(3,0.5,0))
    axis(side=3, at=xaxis, lty="dotted", lwd=0.5, tck=TRUE, las=1, cex.axis=cex.axis)
    par(mgp = c(3,2,0))
    if(pattern) {
        legend("bottomright", legend=rownames(x)[p:1], fill=col[p:1], density=density[p:1], angle=angle[p:1], bg="white")
    } else {
        legend("bottomright", legend=rownames(x)[p:1], fill=grey.colors(p)[p:1], bg="white")
    }

##  Here can be added text on the top horizontal axis
##  mtext("US$ at 2000 constant prices", side = 3, line=2, , cex=cex.axis, las=1)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4])
    if(obox)
        box(which="outer")

    doSave(outfile=gname, type=type)
    par(oldpar)
}

###########################################################################################
## END OF YEARBOOK 2011

xyrect <- function(x1, y1, x2, y2, horizontal = TRUE, ...)
{
            if (horizontal)
                rect(x1, y1, x2, y2, ...)
            else rect(y1, x1, y2, x2, ...)
}

plot(0)

## Help to select pch symbols for plotting of linecharts
test.pch <- function()
{
    x <- 1:10
    y <- rep(0,length(x))
    leg.pch <- 11:20
    plot(x,1:10, type="n")

    for(i in 1:10)
    {
        y <- y + 1
        lines(x, y)
        points(x, y, pch=leg.pch[i], cex=1.25)
    }
}

obox=FALSE          # whether to drow outer bounding box around the graphs
type <- "pdf"       # type of output
pattern <- TRUE     # for barcharts - wheather to use pattern or shades of gray
plot.bg <- "white"

##
## To embed the fonts in the pdf file infile (without extension)
##  - First infile.pdf is converted to PS using pdftops.exe tool
##  - Then embedFots function is used to embed the fonts (calling in turn gswin32c.exe)
##  - And the file is converted back to PDF using ps2pdf.exe
##
doEmbed <- function(infile, outfile)
{
    pdffile <- paste(outfile, ".pdf", sep="")

    ifile <- paste(infile, ".pdf", sep="")
    psfile <- paste(infile, ".ps", sep="")
    cmd <- paste("pdftops", ifile, psfile)
    system(cmd)

    ofile <- paste(outfile, ".ps", sep="")
    embedFonts(file=psfile, outfile=ofile, options="-dUseCIEColor=true -dPDFSETTINGS=/prepress")

    cmd <- paste("ps2pdf", ofile, pdffile)
    system(cmd)
}

doSave2009 <- function(outfile, type)
{
    savePlot(file=outfile, type=type)
    savePlot(file=paste("1",outfile,sep=""), type=type)

    if(type == "ps")
    {
        ifile <- paste(outfile, "ps", sep="")
        ofile <- "xxx.ps"
        embedFonts(file=ifile, outfile=ofile, options="-dUseCIEColor=true -dPDFSETTINGS=/prepress")

        pdffile <- paste(outfile, ".pdf", sep="")
        cmd <- paste("ps2pdf", ofile, pdffile)
        system(cmd)
    }

    if(type == "pdf")
    {
        pdffile <- paste(outfile, ".pdf", sep="")
        psfile <- paste(outfile, ".ps", sep="")
        cmd <- paste("pdftops -paper match", pdffile, psfile)
        system(cmd)

        tmpps <- "xxx.ps"
        embedFonts(file=psfile, outfile=tmpps, options="-dUseCIEColor=true -dPDFSETTINGS=/prepress")

        cmd <- paste("ps2pdf",psfile, pdffile)
        system(cmd)
    }
}

## Barchart: Industrialized-Developing
mvashare <- function(x, obox=FALSE, type="pdf")
{
    ## Read the data
    ##x <- read.table(file="mvashare.tab", header=FALSE)
    ##rownames(x) <- c("Industrialized", "Developing", "China")
    ##colnames(x) <- c(1990, 1995, 2000, 2005)
    ##x <- x[1:2,]

    n <- nrow(x)
    x <- as.matrix(x)

    dev.off()
    windows(8,5)
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))
    par(bg=plot.bg)

    density <- c(25, 100)
    angle <- c(45, 0)

    if(pattern) {
        barplot(x[1:2,], ylim=c(0,100), beside=TRUE, axes=FALSE, legend.text=rownames(x), border="black", col=c("black", "lightgray"), density=density, angle=angle)
    } else {
        barplot(x[1:2,], ylim=c(0,100), beside=TRUE, axes=FALSE, legend.text=rownames(x), border="black")
    }

    par(mgp = c(3,0.5,0))
    ylab1 <- pretty(c(0,110), n=6)
    axis(side=2, at=ylab1, ylab=ylab1, lty="dotted", lwd=0.5, tck=TRUE, las=1)
    mtext("in % of world total", side = 2, line=2, , cex=1)
    par(mgp = c(3,2,0))

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4])
    if(obox)
        box(which="outer")

    doSave(outfile="mvashare", type=type)
    par(oldpar)
}

## Linechart: Growth of MVA per-capita by development groups at constant prices of 2000 (1990=100).
##
mvacap <- function(x, obox=FALSE, type="pdf")
{
    n <- nrow(x)
    p <- ncol(x)
    if(p != p)
        stop("X must have 3 columns: Industrialized, Developing and LDCs")

    xlab <- rownames(x)
    tt <- as.numeric(xlab)

    leg.text <- colnames(x)
    leg.lty <- rep("solid", p)
    leg.pch=c(12,19,21,24)[1:p]
    leg.col=rep("black", p)                  #    leg.bg <- c("lightcyan", "coral", "magenta")
    leg.bg <- rep("white", p)
    plot.bg <- "white"

    dev.off()
    windows(8,5)
    par(bg=plot.bg)

    ## alter margin 4; others are default
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))

    xlim <- c(min(tt),max(tt))
    ylim <- c(min(x), max(x))
    ylim[1] <- ylim[1] - 0.1*ylim[1]
    ylim[2] <- ylim[2] + 0.1*ylim[2]

    plot(tt, x[,1], xlab="", cex.lab=1.2, ylab="",
                xlim=xlim, ylim=ylim, type="n",
                axes=FALSE)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col="white", border="black")
    if(obox)
        box(which="outer")

##  R does not accept rotation of the lables by e.g. srt=90
    axis(side=1, at=pretty(tt, n=14), las=2)
##    axis(side=1, at=pretty(tt,14), lty="dotted", lwd=0.5, tck=TRUE, las=2)

##    axis(side=1, at=tt, labels=FALSE)   # plot the axis without labels
##      Now draw the textual axis labels
##    text(tt+0.3, par("usr")[3] - 7, labels = xlab, srt = 90, pos = 2, xpd = TRUE)

    xaxis <- pretty(range(x), n=6)
    axis(side=2, at=xaxis, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    ind <- 1
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    ind <- 2
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    ind <- 3
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    legend("topleft", legend=leg.text, lty=leg.lty, pch=leg.pch, col=leg.col, bg="white")

    doSave(outfile="mvacap", type=type)
}

## Horizontal Barchart: 1990-2007, 20 countries
top20 <- function(x, obox=FALSE, type="pdf", outfile){
    n <- nrow(x)
    p <- ncol(x)
    x <- t(x)

    dev.off()
    windows(5,8)

    ## alter margin 4; others are default
    oldpar <- par(mar = c(1.1, 8.5, 3.1, 1.1))
    par(las=2)

    xlim<-c(0, max(x)+0.20*max(x))

    density <- c(30, 100)
    angle <- c(45, 90)
    space <- c(0,0.5)
    col <- c("black", "lightgray")
    cex.axis <- 0.9

    if(pattern) {
        barplot(x, width=rep(4,2*n), space=space, beside=TRUE, axes=FALSE, horiz=TRUE, border="black", xlim=xlim, col=col, density=density, angle=angle)
    } else {
        barplot(x, width=rep(4,2*n), space=space, beside=TRUE, axes=FALSE, horiz=TRUE, border="black", xlim=xlim)
    }

    xaxis <- pretty(xlim, n=6)
    par(mgp = c(3,0.5,0))
    axis(side=3, at=xaxis, lty="dotted", lwd=0.5, tck=TRUE, las=1, cex.axis=cex.axis)
    par(mgp = c(3,2,0))
    if(pattern) {
        legend("bottomright", legend=rownames(x)[p:1], fill=col[p:1], density=density[p:1], angle=angle[p:1], bg="white")
    } else {
        legend("bottomright", legend=rownames(x)[p:1], fill=grey.colors(p)[p:1], bg="white")
    }

    mtext("US$ at 2000 constant prices", side = 3, line=2, , cex=cex.axis, las=1)
    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4])
    if(obox)
        box(which="outer")

    doSave(outfile=outfile, type=type)
    par(oldpar)
}

top20.ind <- function(x, obox=FALSE, type="pdf"){
    top20(x, obox, type, "top20ind")
}

top20.dev <- function(x, obox=FALSE, type="pdf"){
    top20(x, obox, type, "top20dev")
}


## Linechart: GDP and MVA growth of developing countries.
GDP.dev <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- 1990:2010
    cgrps <- c(903)

    GDP <- select.gdp(years, cgrps)       # select countries and years
    MVA <- select.mva(years, cgrps)       # select countries and years
    x <- cbind.data.frame(GDP, MVA)
    rownames(x) <- years

    n <- nrow(x)
    p <- ncol(x)

    for(i in 1:p)
        x[,i] <- 100*x[,i]/x[1,i]

    xlab <- rownames(x)
    tt <- as.numeric(xlab)

    leg.text <- colnames(x)
    leg.lty <- c("solid", "solid")
    leg.pch=c(19,21)
    leg.col=c("black","black")
##    leg.bg <- c("lightcyan", "coral")
    leg.bg <- c("white", "white")
    plot.bg <- "white"


    dev.off()
    windows(8,5)
    par(bg=plot.bg)

    ## alter margin 4; others are default
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))

    xlim <- c(min(tt),max(tt))
    ylim <- c(min(x), max(x))
    ylim[1] <- ylim[1] - 0.1*ylim[1]
    ylim[2] <- ylim[2] + 0.1*ylim[2]

    plot(tt, x[,1], xlab="", cex.lab=1.2, ylab="",
                xlim=xlim, ylim=ylim, type="n",
                axes=FALSE)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col="white", border="black")
    if(obox)
        box(which="outer")

##    title(main="Growth of MVA per-capita by development groups at constant prices of 2000\n1990=100.0", cex.main=0.9)
##    title(ylab="", cex.lab=1.2)

##  R does not accept rotation of the lables by e.g. srt=90
    axis(side=1, at=tt, lab=xlab, las=2)

##    axis(side=1, at=tt, labels=FALSE)   # plot the axis without labels
##      Now draw the textual axis labels
##    text(tt+0.3, par("usr")[3] - 7, labels = xlab, srt = 90, pos = 2, xpd = TRUE)

    ylab1 <- pretty(range(x), n=8)
    axis(side=2, at=ylab1, ylab=ylab1, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    ind <- 1
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    ind <- 2
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    legend("topleft", legend=leg.text, lty=leg.lty, pch=leg.pch, col=leg.col, bg="white")
    doSave(outfile="gdpdev", type=type)
}

MVA.major <- function(obox=FALSE, type="pdf")
{
    ## Read the data
    x <- read.table(file="mvamajor.tab", header=TRUE)
    n <- nrow(x)
    rownames(x) <- 1990:(1990+n-1)

    n <- nrow(x)
    p <- ncol(x)
    if(p != 3)
        stop("X must have 3 columns: Industrialized, Developing and LDCs")

    xlab <- rownames(x)
    tt <- as.numeric(xlab)

    leg.text <- colnames(x)
    leg.lty <- c("solid", "solid", "solid")
    leg.pch=c(12,19,21)
    leg.col=c("black","black","black")
##    leg.bg <- c("lightcyan", "coral", "magenta")
    leg.bg <- c("white", "white", "white")
    plot.bg <- "white"


    dev.off()
    windows(8,5)
    par(bg=plot.bg)

    ## alter margin 4; others are default
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))

    xlim <- c(min(tt),max(tt))
    ylim <- c(min(x), max(x))
    ylim[1] <- ylim[1] - 0.1*ylim[1]
    ylim[2] <- ylim[2] + 0.1*ylim[2]

    plot(tt, x[,1], xlab="", cex.lab=1.2, ylab="",
                xlim=xlim, ylim=ylim, type="n",
                axes=FALSE)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col="white", border="black")
    if(obox)
        box(which="outer")

##    title(main="Growth of MVA per-capita by development groups at constant prices of 2000\n1990=100.0", cex.main=0.9)
##    title(ylab="", cex.lab=1.2)

##  R does not accept rotation of the lables by e.g. srt=90
    axis(side=1, at=tt, lab=xlab, las=2)

##    axis(side=1, at=tt, labels=FALSE)   # plot the axis without labels
##      Now draw the textual axis labels
##    text(tt+0.3, par("usr")[3] - 7, labels = xlab, srt = 90, pos = 2, xpd = TRUE)

    ylab1 <- pretty(range(x), n=8)
    axis(side=2, at=ylab1, ylab=ylab1, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    ind <- 1
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    ind <- 2
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    ind <- 3
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    legend("topleft", legend=leg.text, lty=leg.lty, pch=leg.pch, col=leg.col, bg="white")
    doSave(outfile="mvamajor", type=type)
}

## Barchart (stacked bars): Distribution of total MVA of developing countries.
MVA.distr <- function(x, obox=FALSE, type="pdf")
{

    ## Read the data
    ##x <- read.table(file="mvadistr.tab", header=FALSE)
    ##rownames(x) <- x[,1]
    ##x <- as.matrix(x[,-1])
    ##colnames(x) <- c(1990, 1995, 2000, 2005)

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    # rearange the bars:
    ind <-c(2,1,4,5,3)

    dev.off()
    windows(8,5)

    ## alter margins
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))
    par(bg=plot.bg)

    width <- 0.5
    density <- c(25, 100, 25, 100, 25)
    angle <- c(45, 0, 135, 0, 90)
    col <- grey.colors(n+1)[2:(n+1)]
    col.pattern <- c("black", col[1], "black", col[4], "black")

    if(pattern) {
        bp <-  barplot(x[ind,], ylim=c(0,100), width=width, space=2, beside=FALSE, axes=FALSE, border="black", col=col.pattern, density=density, angle=angle)
    } else {
        bp <-  barplot(x[ind,], ylim=c(0,100), width=width, space=2, beside=FALSE, axes=FALSE, border="black", col=col)
    }

    ylab1 <- pretty(c(0,110), n=8)
    axis(side=2, at=ylab1, ylab=ylab1, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    bp1 <- bp - width/2
    bp2 <- bp + width/2
    xx <- x[ind,]
    x0 <- rep(0,p)

    for(i in 1:(n-1))
    {
        x0 <- x0 + xx[i,]
        for(j in 1:(p-1))
            lines(c(bp2[j],bp1[j+1]), x0[j:(j+1)])
    }


    if(pattern) {
        legend("bottomright", legend=rownames(x[ind,])[n:1], bg="white", fill=col.pattern[n:1], density=density[n:1], angle=angle[n:1])
    } else {
        legend("bottomright", legend=rownames(x[ind,])[n:1], fill=col[n:1], bg="white")
    }

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4])
    if(obox)
        box(which="outer")

    doSave(outfile="mvadistr", type=type)
    par(oldpar)
}

##################################################################
##
## Graphs added for Yearbook 2010
##  VT::20.11.2009


graph7 <- function(x, obox=FALSE, type="pdf", at.x)
{

    x <- t(x)   # x was created with 2 rows and many columns (years)
    n <- nrow(x)
    p <- ncol(x)
    if(p != 2)
        stop("X must have 2 columns: Industrialized and Developing")

    xlab <- rownames(x)
    tt <- as.numeric(xlab)

    leg.text <- colnames(x)
    leg.lty <- rep("solid", 3)
    leg.pch=c(12,13,1)    #c(21,19,24)
    leg.col=rep("black",3)                  #    leg.bg <- c("lightcyan", "coral", "magenta")
    leg.bg <- rep("white", 3)
    plot.bg <- "white"


    dev.off()
    windows(8,5)
    par(bg=plot.bg)

    ## alter margin 4; others are default
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))

    xlim <- c(min(tt),max(tt))
    ylim <- c(min(x), max(x))
    ylim[1] <- ylim[1] - 0.1*ylim[1]
    ylim[2] <- ylim[2] + 0.1*ylim[2]

    plot(tt, x[,1], xlab="", cex.lab=1.2, ylab="",
                xlim=xlim, ylim=ylim, type="n",
                axes=FALSE)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col="white", border="black")
    if(obox)
        box(which="outer")

##  R does not accept rotation of the lables by e.g. srt=90
    if(missing(at.x))
        at.x = pretty(tt, n=12)
    las <- if(length(at.x) > length(tt)/2) 2 else 0
    axis(side=1, at=at.x, las=las)

##    axis(side=1, at=pretty(tt,14), lty="dotted", lwd=0.5, tck=TRUE, las=2)

##    axis(side=1, at=tt, labels=FALSE)   # plot the axis without labels
##      Now draw the textual axis labels
##    text(tt+0.3, par("usr")[3] - 7, labels = xlab, srt = 90, pos = 2, xpd = TRUE)

    xaxis <- pretty(range(x), n=6)
    axis(side=2, at=xaxis, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    for(ind in 1:2)
    {
        lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
        points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)
    }

    xdif <- (tt[2] - tt[1])/5
    for(ii in 1:length(tt))
    {
        xmin <- tt[ii] - xdif
        xmax <- tt[ii] + xdif
        ymin <- x[ii,1]
        ymax <- x[ii,2]
        xyrect(xmin,ymin,xmax,ymax, density=15, angle=0)
    }

    legend("topright", legend=leg.text, lty=leg.lty, pch=leg.pch, col=leg.col, bg="white")

    doSave(outfile="graph7", type=type)
}

##
##
## Linechart: Growth of MVA per-capita by development groups at constant prices of 2000 (1990=100).
##
##  Almost the same as graph.2 - see mvacap()
graph8 <- function(x, obox=FALSE, type="pdf")
{
    n <- nrow(x)
    p <- ncol(x)
    if(p != 3)
        stop("X must have 3 columns: African LDCs, All LDCs and All Developing")

    xlab <- rownames(x)
    tt <- as.numeric(xlab)

    leg.text <- colnames(x)
    leg.lty <- rep("solid", 3)
    leg.pch=c(12,19,21)    #c(21,19,24)
    leg.col=rep("black",3)                  #    leg.bg <- c("lightcyan", "coral", "magenta")
    leg.bg <- rep("white", 3)
    plot.bg <- "white"


    dev.off()
    windows(8,5)
    par(bg=plot.bg)

    ## alter margin 4; others are default
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))

    xlim <- c(min(tt),max(tt))
    ylim <- c(min(x), max(x))
    ylim[1] <- ylim[1] - 0.1*ylim[1]
    ylim[2] <- ylim[2] + 0.1*ylim[2]

    plot(tt, x[,1], xlab="", cex.lab=1.2, ylab="",
                xlim=xlim, ylim=ylim, type="n",
                axes=FALSE)

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4], col="white", border="black")
    if(obox)
        box(which="outer")

##  R does not accept rotation of the lables by e.g. srt=90
    axis(side=1, at=pretty(tt, n=14), las=2)
##    axis(side=1, at=pretty(tt,14), lty="dotted", lwd=0.5, tck=TRUE, las=2)

##    axis(side=1, at=tt, labels=FALSE)   # plot the axis without labels
##      Now draw the textual axis labels
##    text(tt+0.3, par("usr")[3] - 7, labels = xlab, srt = 90, pos = 2, xpd = TRUE)

    xaxis <- pretty(range(x), n=6)
    axis(side=2, at=xaxis, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    ind <- 1
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    ind <- 2
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    ind <- 3
    lines(tt, x[,ind], lty=leg.lty[ind], col=leg.col[ind])
    points(tt, x[,ind], pch=leg.pch[ind], bg=leg.bg[ind], cex=1.25)

    legend("topleft", legend=leg.text, lty=leg.lty, pch=leg.pch, col=leg.col, bg="white")

    doSave(outfile="graph8", type=type)
}

##
##  VT::20.11.2009
## Barchart (stacked bars): Distribution of total MVA of developing countries.
##
##  Almost the same as graph #5 - see MVA.distr()
##
graph9 <- function(obox=FALSE, type="pdf")
{
    ## Select and prepare the data
    years <- c(1990, 1995, 2000, 2005, 2009)
    cgrps <- c(916, 915, 990, 917)
    cnames <- c("China", "NICs", "Other developing \ncountries", "LDCs")

    x <- select.mva(years, cgrps)       # select countries and years
    xdev <- select.mva(years, 903)      # select developing countries
    xdev <- as.matrix(xdev) %*% rep(1,4)           #
    x <- round(x/xdev*100, 2)                      # calculate shares in developing countries
    x <- t(x)
    rownames(x) <- cnames

    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)

    dev.off()
    windows(8,5)

    ## alter margins
    oldpar <- par(mar = c(3.1, 3.1, 2.1, 2.1))
    par(bg=plot.bg)

    width <- 0.5

    ind <-1:4
    density <- c(25, 100, 25, 100)
    angle <- c(45, 0, 135, 0)
    col <- grey.colors(n+1)[2:(n+1)]
    col.pattern <- c("black", col[1], "black", col[4])


    if(pattern) {
        bp <-  barplot(x[ind,], ylim=c(0,100), width=width, space=2, beside=FALSE, axes=FALSE, border="black", col=col.pattern, density=density, angle=angle)
    } else {
        bp <-  barplot(x[ind,], ylim=c(0,100), width=width, space=2, beside=FALSE, axes=FALSE, border="black", col=col)
    }

    ylab1 <- pretty(c(0,110), n=8)
    axis(side=2, at=ylab1, ylab=ylab1, lty="dotted", lwd=0.5, tck=TRUE, las=1)

    bp1 <- bp - width/2
    bp2 <- bp + width/2
    xx <- x[ind,]
    x0 <- rep(0,p)

    for(i in 1:(n-1))
    {
        x0 <- x0 + xx[i,]
        for(j in 1:(p-1))
            lines(c(bp2[j],bp1[j+1]), x0[j:(j+1)])
    }


    if(pattern) {
        legend("bottomright", legend=rownames(x[ind,])[n:1], bg="white", fill=col.pattern[n:1], density=density[n:1], angle=angle[n:1])
    } else {
        legend("bottomright", legend=rownames(x[ind,])[n:1], fill=col[n:1], bg="white")
    }

    usr <- par("usr")
    rect(usr[1], usr[3], usr[2], usr[4])
    if(obox)
        box(which="outer")

    doSave(outfile="graph9", type=type)
    par(oldpar)
}


##
## Line chart - MVA growth in industrialized regions (1990=100)
##
graph10 <- function(x, obox=FALSE, type="pdf")
{
    ## prepare the data set
    x1 <- cbind(x$CT897, x$CT935, x$CT937, x$CT960)
    for(i in 1:ncol(x1))
        x1[,i] <- x1[,i]/x1[1,i]*100

    rownames(x1) <- x$YEAR
    colnames(x1) <- c("EU-15", "East Asia", "North America", "CIS")
    do.line(x1, outfile="graph10")
}

##
## Barchart - average annual growth in developing countries
##  by geographical region
graph11 <- function(x, obox=FALSE, type="pdf")
{
    ## prepare the data set
    x1 <- cbind(x$CT903, x$CT916, x$CT962-x$CT916, x$CT980, x$CT982, x$CT957)

    rownames(x1) <- x$YEAR
    colnames(x1) <- c("Developing \ncountries",
                      "China",
                      "Asia \n(excl.China)",
                      "Europa",
                      "Africa",
                      "Latin \nAmerica")
    b1 <- ((x1["2004",]/x1["1999",])^(1/5) - 1)*100
    b2 <- ((x1["2009",]/x1["2004",])^(1/5) - 1)*100

    x2 <- t(cbind(b1,b2))
    rownames(x2) <- c("1999-2004", "2004-2009")
    do.bar(x2, outfile="graph11")
}


## embedFonts(file="mvadistr.pdf", outfile = "mvadistrx.pdf")
## gswin32c -dNOPAUSE -dBATCH -q -dAutoRotatePages=/None -sDEVICE=pdfwrite -sOutputFile=c:\\TEMP\\RtmpLZkdzf\\Rembed41bb5af1 -sFONTPATH=  mvadistr.pdf"

####gswin32c -dSAFER -dNOPLATFONTS -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dPDFSETTINGS=/printer -dEmbedAllFonts=true -sOutputFile=c:\.x.pdf mvadistr.pdf
####gswin32c -dSAFER -dNOPLATFONTS -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress  -dEmbedAllFonts=true -sOutputFile=c:\.x.pdf mvadistr.pdf

## - ignores the specified 'outfile' and writes to a TEMP directory

####gswin32c -dSAFER -dNOPLATFONTS -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -dMaxSubsetPct=100 -dSubsetFonts=true -dEmbedAllFonts=true -sOutputFile=c:\.x.pdf -f mvadistr.pdf

####gswin32c -dSAFER -dNOPLATFONTS -dNOPAUSE -dBATCH -dAutoRotatePages=/None -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -dMaxSubsetPct=100 -dSubsetFonts=true -dEmbedAllFonts=true -sOutputFile=c:\.x.pdf -f mvadistr.pdf


## - Generate EPS
## - Call gswin32c on the EPS files with output as PDF

##gswin32c -dSAFER -dNOPLATFONTS -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dPDFSETTINGS=/printer -dEmbedAllFonts=true -sOutputFile=mvadistr.pdf mvadistr.eps

test.embed <- function()
{

    plot(0,0)
    savePlot(file="testembed", type="pdf")
    cc<-embedFonts(file="testembed.pdf", outfile="testout.pdf", options="-dUseCIEColor=true -dPDFSETTINGS=/printer")
}


######################################################################
load("graphdata.rda")

## Used for Yearbook 2014 ===================================
##
if(FALSE)
{
MVA.growth(obox, type)              # 1. uses graph.mva
MVA.LDC.2013(obox, type)            # 2. uses graph.mva
MVA.share(obox, type)               # 3. uses graph.mva
MVA.emp(obox, type)                 # 4. uses locally computed data (exported from INDSTAT and MVA)
MVA.emerging(obox, type)            # 5. uses graph.mva
MVA.numbers(obox, type)             # 6. uses graph.mva
}

## Used for Yearbook 2013 ===================================
##
if(FALSE)
{
MVA.overall.2013(obox, type)        # 1. uses graph.mva
MVA.LDC.2013(obox, type)            # 2. uses graph.mva
MVA.share(obox, type)               # 3. uses graph.mva
MVA.emp(obox, type)                 # 4. uses locally computed data (exported from INDSTAT and MVA)
MVA.emerging(obox, type)            # 5. uses graph.mva
MVA.numbers(obox, type)             # 6. uses graph.mva
}

## Used for Yearbook 2012 ===================================
##
if(FALSE)
{
MVA.overall(obox, type)             # 1. uses graph.mva
MVA.recession(obox, type)           # 2. uses graph.mva
MVACAP.freq()                       # 3. uses graph.mva, graph.pop, all countries
MVA.BRICS(obox, type)               # 4. uses graph.mva
MVACAP.oil(obox, type)              # 5. uses graph.mva, graph.pop, rcodes; selected countries
MVA.LDC(obox, type)                 # 6. uses graph.mva
}

## Used for Yearbook 2011 ===================================
##
if(FALSE)
{
MVA.top10.2011(obox, type)          # 1. uses specialized graph.top
MVA.world.2011(obox, type)          # 2. uses graph.mva
MVA.europe.2011(obox, type)         # 3. uses graph.mva
MVAGDP.share.2011(obox, type)       # 4. uses graph.mva and graph.gdp
MVA.distr.2011(obox, type)          # 5. uses graph.mva
GDPMVA.dev.2011(obox, type)         # 6. uses graph.mva and graph.gdp
}

## Used for Yearbook 2010 ===================================
##
if(FALSE)
{
graph7(x=graph.7, obox, type)
graph8(x=graph.8, obox, type)
graph9(obox, type)                  # uses graph.mva
graph10(x=graph.mva, obox, type)    # uses graph.mva
graph11(x=graph.mva, obox, type)    # uses graph.mva
GDP.dev(obox, type)                 # uses uses graph.mva and graph.gdp
}

## Used for Yearbook 2009 ===================================
##
if(FALSE)
{
mvashare(x=graph.1, obox, type)
mvacap(x=graph.2, obox, type)
top20.ind(x=graph.3, obox, type)
##MVA.major(obox, type)
MVA.distr(x=graph.5, obox, type)
top20.dev(x=graph.6, obox, type)
GDP.dev(obox, type)                 # uses uses graph.mva and graph.gdp
}
