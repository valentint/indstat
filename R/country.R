####
##  Return the index in data.frame 'rcodes' of the
##  country specified by 'ct' where 'ct' can be index, UN code,
##  ISO code or country name
##
##  'ct' can also be a vector or list of codes, the corresponding
##  list of indices is returned
##

## Pad the UN country code with leading 0s
##
padct <- function(x)
    substr(paste("00", x, sep=""), nchar(x), nchar(x)+2)

ncountry <- function()
    nrow(rcodes)

getCountryIndex <- function(ct, groups=FALSE)
{
    ct <- as.character(ct)
    ret <- rep(0, length(ct))

    for(i in 1:length(ct))
    {
        ctx <- ct[[i]]
        if(is.character(ctx))
        {
            if(nchar(ctx) == 2 && is.numeric_like(ctx))
                ctx <- paste("0", ctx, sep="")
            if(groups==FALSE & length(ipos <- which(rcodes$ISOCN == toupper(ctx))) == 1)
                ret[i] = ipos
            else if(length(ipos <- which(rcodes$ACODE == toupper(ctx))) == 1)
                ret[i] <- ipos
            else if(length(ipos <- which(toupper(rcodes$DESC) == toupper(ctx))) == 1)
                    ret[i] <- ipos
            else
                stop(paste("Undefined country code/name: ", ctx))
        }
        stopifnot(ret[i] > 0 & ret[i] <= ncountry())
    }

    unique(ret)
}

existsCountry <- function(ct, groups=FALSE)
{
    ct <- as.character(ct)
    ret <- rep(FALSE, length(ct))

    for(i in 1:length(ct))
    {
        ctx <- ct[[i]]
        if(is.character(ctx))
        {
            if(nchar(ctx) == 2)
                ctx <- paste("0", ctx, sep="")
            if(groups==FALSE & length(ipos <- which(rcodes$ISOCN == toupper(ctx))) == 1)
                ret[i] = ipos
            else if(length(ipos <- which(rcodes$ACODE == toupper(ctx))) == 1)
                ret[i] <- ipos
            else if(length(ipos <- which(toupper(rcodes$DESC) == toupper(ctx))) == 1)
                    ret[i] <- ipos
            else
                ret[1] <- 0
        }
        ret[i] <- ret[i] > 0
    }

    as.logical(ret)
}

getCountryCode <- function(ct)
{
    getCountryCodeUN(ct)
}

getCountryCodeISO3 <- function(ct, groups=FALSE)
{
    ct <- getCountryIndex(ct, groups)
    stopifnot(ct > 0 & ct <= ncountry())

    as.character(rcodes[ct,]$ISOCN)
}

######
##  Return UN code as character with leading '0', e.g. Austria="040" and Bulgaria="100"
##
getCountryCodeUN <- function(ct, groups=FALSE)
{
    ct <- getCountryIndex(ct, groups)
    stopifnot(ct > 0 & ct <= ncountry())

    ctx <- as.character(rcodes[ct,]$ACODE)
    sapply(ctx, FUN=function(x) if(nchar(x) == 2) paste("0",x,sep="") else x)
}

getCountryName <- function(ct, groups=FALSE)
{
    ct <- getCountryIndex(ct, groups)
    stopifnot(ct > 0 & ct <= ncountry())

    as.character(rcodes[ct,]$DESC)
}

## Returns a list of country codes belonging to the country group 'ct'
getCountryGroup <- function(ct)
{
    xgr <- raggr[which(raggr$ACODE %in% ct), c(4, 5, 1, 6)]
    xgr$CCODE
}

## Returns a data frame with the countries belonging to the country group 'ct'
##  - a data frame with country code, country name, ISO3 code, groupe code and group name
getCountryListByGroup <- function(ct)
{
    xgr <- raggr[which(raggr$ACODE %in% ct), ]
    xgr <- merge(xgr, rcodes[,c("ACODE", "DESC")], by="ACODE")

    xgr[c("CCODE", "CDESC", "ISOCN", "ACODE", "ADESC")]
}

##  Add additional information (metadata) to a data frame df containing
##      country information.
##
addCountryName <- function(df, ct.x="CT", ct.y="ACODE", ct.name="NAME", comptype=FALSE)
{
    stopifnot(is.data.frame(df))
    stopifnot(ct.x %in% colnames(df))
    stopifnot(ct.y %in% colnames(rcodes))

    if("COMPTYPE" %in% colnames(df))
        comptype=FALSE

    mycodes <- if(comptype) rcodes[,c(ct.y, "DESC", "COMPTYPE")] else rcodes[,c(ct.y, "DESC")]
    colnames(mycodes)[2] <- ct.name
    merge(df, mycodes, by.x=ct.x, by.y=ct.y)
}

addCountryISO3 <- function(df, ct.x="CT", ct.y="ACODE", ct.iso3="ISO3", comptype=FALSE)
{
    stopifnot(is.data.frame(df))
    stopifnot(ct.x %in% colnames(df))
    stopifnot(ct.y %in% colnames(rcodes))

    if("COMPTYPE" %in% colnames(df))
        comptype=FALSE

    mycodes <- if(comptype) rcodes[,c(ct.y, "ISOCN", "COMPTYPE")] else rcodes[,c(ct.y, "ISOCN")]
    colnames(mycodes)[2] <- ct.iso3
    merge(df, mycodes, by.x=ct.x, by.y=ct.y)
}

## Returns a list of all countries in the database
getCountryList <- function()
{
    ct <- as.character(rcodes[rcodes$COMPTYPE == "M",]$ACODE)
    ##  ct <- ct[which(ct %in% graph.base$CT)]                  # VT::10.12.2021 - here we do not have the graph.base data
    ct
}
