##------------------------------------------------------------------
## Plot a comparison of an indicator in the current and previous
##  file into a PDF
##
##  Parameters:
##  - df1 and df2: two dataframes with fields country and year
##  - var: the variale to compare
##  - labnew and labold: labels for the legend
##  - fout: the output file
##  - ctlist: optional country list to use (if missing the list of
##      all countries in both files will be used)
##
##  Returns:
##  - xout: a data frame of the comparison

compare_plot <- function(df1, df2, var, labnew=2020, labold=2019, suffix="", fout=paste0("compare-", var, suffix, ".pdf"), ctlist, legend.position="topleft", xlab, ylab, ylim_min)
{
    if(length(which(colnames(df1) == "country")) == 0 ||
        length(which(colnames(df2) == "country")) == 0)
        stop("Need a field 'country' in both data frames!")

    if(length(which(colnames(df1) == "year")) == 0 ||
        length(which(colnames(df2) == "year")) == 0)
        stop("Need a field 'year' in both data frames!")

    if(length(which(colnames(df1) == var)) == 0 ||
        length(which(colnames(df2) == var)) == 0)
        stop(paste0("Need a field '", var, "' in both data frames to compare on!"))

    if(is.factor(df1$country))
        df1$country <- as.character(df1$country)
    if(is.factor(df2$country))
        df2$country <- as.character(df2$country)

    ## If no country list provided, take the union of the countries
    ##  in the two data frames
    if(missing(ctlist))
        ctlist <- sort(unique(c(df1$country, df2$country)))

    if(missing(xlab))
        xlab <- ""
    if(missing(ylab))
        ylab <- ""

    xlim <- c(min(c(df1$year, df2$year)), max(c(df1$year, df2$year)))

    pdf(file=fout)
    par(mfrow = c(3,2))     # 6 graphs on each page
    xout <- NULL
    for(ct in ctlist)
    {
        cat("\nCountry:", ct, "\n")

        x1 <- df1[which(df1$country == ct),]
        x2 <- df2[which(df2$country == ct),]
        xx <- merge(x1[, c("country", "cname", "year", var)], x2[, c("country", "cname", "year", var)], by=c("country", "year"), all=TRUE)

        ## Changed names
        if(!is.na(xx$cname.x[1]))
        {
            if(is.na(xx$cname.y[1]) || xx$cname.x[1] != xx$cname.y[1])
                xx$cname.y <- xx$cname.x
        } else
        {
            xx$cname.x <- xx$cname.y
        }

##        cat("\nCountry:", ct, "=", xx$cname.x[1], "\n")

        ## Differences in the values
        var1 <- paste0(var, ".x")   # new
        var2 <- paste0(var, ".y")   # old
        xx$diff <- round(100*(xx[, var1] - xx[, var2])/xx[, var2],)

        print(xx$diff)
        nobs <- nrow(xx[!is.na(xx[, var1]) & !is.na(xx[, var2]), ])
        diff <- abs(median(xx$diff, na.rm=TRUE))
        cor <- ifelse(all(is.na(xx[, var1] + xx[, var2])),
                NA,
                cor(xx[, var1], xx[, var2], use="complete.obs"))
        cat("\nCountry:", ct, "=", xx$cname.x[1], dim(x1)[1], dim(x2)[1], cor, nobs, "\n")

        ## add to the output
        xout <- rbind(xout, xx)

        ## plot it
        y <- c(x1[, var], x2[, var])
        ylim <-  if(all(is.na(y)))              c(0,1)
                 else if(!missing(ylim_min))    c(ylim_min, max(y, na.rm=TRUE))
                 else                           c(min(y, na.rm=TRUE), max(y, na.rm=TRUE))

        x <- if(nrow(x1) > 0) x1 else x2
        plot(x[, c("year", var)], type="n", ylim=ylim, xlim=xlim, xlab=xlab, ylab=ylab)

        lines(x1[, c("year", var)], col="red")      # new line
        lines(x2[, c("year", var)], col="blue")     # old line

        ctname <- if(existsCountry(ct, groups=TRUE)) substr(getCountryName(ct), 1, 20) else ct
        print(ctname)
        title(main=paste0(toupper(var), ": ", ctname, " [cor=", round(cor, 2), ", med diff=", diff, "%, nobs=", nobs,  "]"), cex.main=0.8)
        if(!is.null(legend.position))
            legend(legend.position, legend=c(labnew, labold), lty=1, col=c("red", "blue"))
    }
    dev.off()

    xout
}
