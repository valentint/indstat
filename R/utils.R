######
##  VT::08.01.2021
##
##  roxygen2::roxygenise("C:/users/valen/onedrive/myrepo/R/indstat", load_code=roxygen2:::load_installed)
##
##  check if a str obj is actually numeric
##
##  From https://stackoverflow.com/questions/13638377/test-for-numeric-elements-in-a-character-string
##
#' @name utils
#' @title check if a string is actually numeric
#' @description check if a str obj is actually numeric
#' @param x a str vector, or a factor of str vector, or numeric vector. x will be coerced and trimws.
#' @param na.strings case sensitive strings that will be treated to NA.
#' @param naAsTrue whether NA (including actual NA and na.strings) will be treated as numeric like
#' @return a logical vector (vectorized).
#' @export
#' @note Using regular expression
#' \cr TRUE for any actual numeric c(3,4,5,9.9) or c("-3","+4.4",   "-42","4L","9L",   "1.36e4","1.36E4",    NA, "NA", "","NaN", NaN):
#' \cr positive or negative numbers with no more than one decimal c("-3","+4.4") OR
#' \cr positive or negative integers (e.g., c("-42","4L","39L")) OR
#' \cr positive or negative numbers in scientific notation c("1.36e4","1.36E4")
#' \cr NA, or na.strings
NULL

#' @rdname utils
#' @export
is.numeric_like <- function(x,naAsTrue=TRUE,na.strings=c('','.','NA','na','N/A','n/a','NaN','nan')){
    x = trimws(x,'both')
    x[x %in% na.strings] = NA
    # https://stackoverflow.com/a/21154566/2292993
    result = grepl("^[\\-\\+]?[0-9]+[\\.]?[0-9]*$|^[\\-\\+]?[0-9]+[L]?$|^[\\-\\+]?[0-9]+[\\.]?[0-9]*[eE][0-9]+$",x,perl=TRUE)
    if (naAsTrue) result = result | is.na(x)
    return((result))
}

#################################################################
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
##  EXAMPLE: Africa MVA 2003-2013
##
##  T <- 2003:2013
##  x <- c(97693898, 101575425, 106757274, 112278366, 119028345, 123812478, 121168320, 126346068, 128559983, 133599663, 138784208)
##  names(x) <- T
##
##  t0 <- 2003; ipos1 <- 1
##  tn <- 2007; ipos2 <- 5
##  (x[ipos2]/x[ipos1])^(1/(tn-t0))*100 - 100
##  grate(x, T, t0, tn)
##
##  t0 <- 2008; ipos1 <- 6
##  tn <- 2012; ipos2 <- 10
##  (x[ipos2]/x[ipos1])^(1/(tn-t0))*100 - 100
##  grate(x, T, t0, tn)
##
grate <- function(X, T, t0, tn)
{
    ipos1 <- which(T==t0)
    ipos2 <- which(T==tn)
    vn <- X[ipos2]
    v0 <- X[ipos1]
    x <- X[(ipos1-1):ipos2]
    t <- T[(ipos1-1):ipos2]
##    cat("\nPeriod=",T[ipos1], "-", T[ipos2],"\n")

    ## 1. "One year" growth rate
    r.one <- 100*(vn - v0)/v0/(tn-t0)

    ## 2. "Average" growth rate
    rx2 <- x[2:length(x)]
    rx1 <- x[1:length(x)-1]
    rx <- 100*(rx2-rx1)/rx1
    r.ave <- mean(rx)
    names(rx) <- t[-1]

    ## 3. Copound growth rate
    ## CAGR(tn,t0) = [V(tn)/V(t0)]^(1/(tn-t0)) - 1
    r.compound <- 100 *((vn/v0)^(1/(tn-t0)) - 1)
##    cat("\n", t0, tn, v0, vn, r2, "\n")


    ## 4. Semi-log linear OLS regression
    x <- X[ipos1:ipos2]
    t <- T[ipos1:ipos2]
    if(length(which(is.na(x))) == 0)
    {
        lx <- log(ifelse(x>0, x, 1e-6))             # avoid non-positive numbers
        lmx <- lm(lx~t)
        r.regress <- 100*(exp(coef(lmx)[2])-1)
    } else
    {
        r.regress <- NA
        lmx <- NA
    }

    r.sd <- sd(rx)
    r.volatility <- sqrt(mean((rx-rep(r.compound, length(rx)))^2))

    ret <- list(t0=t0, tn=tn, r.one=r.one, r.annual=rx, r.ave=r.ave, r.sd=r.sd, r.volatility=r.volatility, r.compound=r.compound, r.regress=r.regress, lm=lmx)
    class(ret) <- "grate"
    ret
}

print.grate <- function(x, ...)
{
    cat("\nAAGR for the period ", x$t0, "-", x$tn, "\n")
    c1 <- c("r.one", "r.ave", "r.compound", "r.regress")
    c2 <- c(x$r.one, x$r.ave, x$r.compound, x$r.regress)
    df <- cbind.data.frame(c2)
    rownames(df) <- c1
    colnames(df) <- "AAGR"
    print(round(df, 2))
    cat("\nAnnual growth rates: \n")
    print(round(x$r.annual,2))
    cat("\nSTD       ", round(x$r.sd,2), "\nVolatility", round(x$r.volatility,2), "\n")
}

## From CRAN - TeachingDemos
col2gray <- function (cols)
{
    col2grey(cols)
}
col2grey <- function (cols)
{
    rgb <- col2rgb(cols)
    gry <- rbind(c(0.3, 0.59, 0.11)) %*% rgb
    rgb(gry, gry, gry, maxColorValue = 255)
}
