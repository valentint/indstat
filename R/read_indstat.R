##-------------------------------------------------------------------------------
##
##  roxygen2::roxygenise("MY PATH", load_code=roxygen2:::load_installed)
##

##
##  - The input file name: we assume the following convention for
##  the INDSTAT data file name:
##    instRR$$ - where RR is the revision and the database (INDSTAT 4,
##      ISIC Rev. 3; INDSTAT 4, ISIC Rev. 4 or INDSTAT 4 (3x, 4x or 32)
##      and 'us' following means that the database was downloaded in USD,
##      not in national currency.
##  - There is slight difference betwen the format of the data portal
##      and the deskktop application - use the parameter 'source'
##  for this purpose
##
read.indstat <- function(indata, rev=c("3x", "4x", "32", "3xus", "4xus", "32us"),
    source=c("data-portal", "desktop"))
{
    rev <- match.arg(rev)
    source <- match.arg(source)
    revision <- substr(rev, 1, 2)
    digits <- if(substr(rev, 2, 2) == "x") "4" else "2"

    fname <- file.path(indata, paste0("inst", rev, ".csv"))
    cat("\nReading INDSTAT", digits, "revision", substr(revision, 1, 1), "from file", fname, "\n")

    if(source == "desktop") {
        inst.classes <- c("character","character","integer","character","character","numeric","character","character","integer","character")
        inst.names   <- c("ctable","country","year","isic","isiccomb","value","utable","source","lastupdated","unit")
    } else {
        inst.classes <- c("character","character","integer","character","character","numeric","character","character","character")
        inst.names   <- c("ctable","country","year","isic","isiccomb","value","utable","source","unit")
    }

    inst <- read.csv(file=fname, header=FALSE, col.names=inst.names,
        colClasses=inst.classes, na.strings="...", stringsAsFactors=FALSE)
    dim(inst)
    head(inst)

    inst
}

read.indstat2 <- function(indata, fdb=c("32", "3x", "4x", "32us", "3xus", "4xus"), source=c("data-portal", "desktop"), first_year, last_year)
{

    fname <- file.path(indata, paste0("inst", fdb, ".csv"))

    require(reshape2)

    if(source == "desktop")
    {
        inst.classes <- c("character","character","character","character","character","numeric","character","character","integer","character")
        inst.names   <- c("ctable","country","year","isic","isiccomb","value","utable","source","lastupdated","unit")
    } else
    {
        inst.classes <- c("character","character","integer","character","character","numeric","character","character","character")
        inst.names   <- c("ctable","country","year","isic","isiccomb","value","utable","source","unit")
    }

    inst         <- read.csv(file=fname, header=FALSE, col.names=inst.names, colClasses=inst.classes, na.strings="...")
    dim(inst)
    head(inst)
    instx <- inst[, c(1:4,6)]
    instx <- instx[instx$ctable != 51, ]         # exclude the index numbers

    if(!missing(first_year))
        instx <- instx[instx$year >= first_year, ]

    if(!missing(last_year))
        instx <- instx[instx$year <= last_year, ]

    instxx <- dcast(country+year+isic~ctable, data=instx)
    head(instxx)
    colnames(instxx)[4:10] <- c("EST", "EMP","WS","OUT","VA", "GFCF", "FEM")
    instxx <- instxx[, c(1:8, 10)]
    head(instxx)

    fout <- file.path(indata, paste0("inst", fdb, ".rda"))
    var <- paste0("inst", fdb)

    assign(var, instxx)

    if(fdb=="3x")       save(inst3x, file=fout)
    else if (fdb=="4x") save(inst4x, file=fout)
    else if (fdb=="32") save(inst32, file=fout)
    else if (fdb=="3xus") save(inst3xus, file=fout)
    else if (fdb=="4xus") save(inst4xus, file=fout)
    else if (fdb=="32us") save(inst32us, file=fout)

    instxx
}

#'
#' Read all three INDSTAT data files and return an object containing three data frames.
#'
#' @param indata Directory where to find the data files, usually
#'  this is \code{file.path(home, "Data"} where \code{home} is the project root directory.
#' @param rev_suffix The input INDSTAT files can be downloaded in
#'  national currency and in USD, thus \code{rev_suffix="us"} means
#'  that we are reading files in USD (default) and \code{rev_suffix=""} - national currency.
#'
#'  We assume the following file names names of the databases:
#'      \itemize{
#'          \item inst32.csv      - INDSTAT 2, in national currency
#'          \item inst32us.csv    - INDSTAT 2, in USD
#'          \item inst3x.csv      - INDSTAT 4, ISIC Revision 3, in national currency
#'          \item inst3xus.csv    - INDSTAT 4, ISIC Revision 3, in USD
#'          \item inst4x.csv      - INDSTAT 4, ISIC Revision 4, in national currency
#'          \item inst4xus.csv    - INDSTAT 4, ISIC Revision 4, in USD
#'      }
#'
#' @param source There is minor difference in the file format downloaded
#'  from the data portal \url{stat.unido.org} and from the desktop
#'  application. \code{source="data=portal"} (default) will read files
#'  downloaded from the data protal.
#' @param first_year_3x, first_year_4x, first_year_32, first_year Where to
#'  start reading the files - can be different for the three databases. The
#' default is \code{first_year_3x=2010, first_year_4x=2008, first_year_32=2000, first_year=first_year_3x}.
#' @param exclude_IIP whether to exclude the IIP data from INDSTAT 2, default is \code{exclude_IIP=TRUE}.
#' @return An S3 object (a list) containing simply the three data
#'  frames: inst32, inst3x and inst4x as well as the vectors of
#'  country codes lists for each of the databases: ct32, ct3x and ct4x.
#'  These are given just for convenience, you can easily compute them, for example:
#'  ct32 <- unique(inst32$country)
#'
#' @examples
#'
#'  dd <- read_data(indata)
#'  dim(dd$inst32)
#'  head(dd$inst32)
#'
#' @export
#' @author \email{valentin@@todorov.at}
#'
read_data <- function(indata, rev_suffix=c("us", "none"), source=c("data-portal", "desktop"),
        first_year_3x=2008, first_year_4x=2008, first_year_32=2000, first_year=first_year_3x,
        exclude_IIP=TRUE)
{
    rev_suffix <- match.arg(rev_suffix)
    if(rev_suffix == "none")
        rev_suffix=""
    source <- match.arg(source)

    inst32 <- read.indstat(indata, rev=paste0("32", rev_suffix), source=source)
    inst3x <- read.indstat(indata, rev=paste0("3x", rev_suffix), source=source)
    inst4x <- read.indstat(indata, rev=paste0("4x", rev_suffix), source=source)

    inst32 <- inst32[inst32$year >= first_year, ]
    if(exclude_IIP)
        inst32 <- inst32[inst32$ctable != 51, ]         # exclude the index numbers
    inst3x <- inst3x[inst3x$year >= first_year_3x, ]
    inst4x <- inst4x[inst4x$year >= first_year_4x, ]

    ## Which countries report only in ISIC Revision 3 - reduce inst3x to these countries
    (ct32 <- unique(inst32$country))
    getCountryName(ct32)

    (ct3x <- unique(inst3x$country))
    getCountryName(ct3x)

    ct4x <- unique(inst4x$country)
    (ct3x <- ct3x[!(ct3x %in% ct4x)])                       # countries which do not report in R4
    getCountryName(ct3x)

    inst3x <- inst3x[which(inst3x$country %in% ct3x), ]     # including only the countries which do not report in R4
    cat("\nINDSTAT 4, Revision 3 countries by year:")
    print(table(inst3x$country, inst3x$year))


    ## Are there countries in INDSTAT 2 which are neither in INDSTAT 4 Revision 4 nor in INDSTAT 4 Revision 3
    ## (in the selected time intervals)
    if(length(ctx <- ct32[which(!(ct32 %in% c(ct4x, ct3x)))]) > 0) {
        cat("\n", getCountryName(ctx))
    } else {
        cat("\nThere are no countries in INDSTAT2 which are not included in INDSTAT 4 (R4 and R3)\n")
    }

    list(inst32=inst32, inst3x=inst3x, inst4x=inst4x, ct32=ct32, ct3x=ct3x, ct4x=ct4x)
}


cast_indstat <- function(inst)
{
    instx <- inst[, c(1:4, 6)]                                  # Select ctable, country, year, isic and value
    instx <- dcast(country+year+isic~ctable, data=instx)        # cast to wide format with all variables
    colnames(instx)[4:10] <- c("EST", "EMP","WS","OUT","VA",    # rename the columns
            "GFCF", "FEM")
    instx <- instx[, c(1:8, 10)]                                # skip GFCF
    instx
}

#'
#' Read the MVA database from an Excel file and return a data frame containing only the countries.
#'
#' @param indata Directory where to find the data files, usually
#'  this is \code{file.path(home, "Data"} where \code{home} is the project root directory.
#' @param prod_year The production year. This is necessary to form
#'  the name of the Excel file, which is usually MVAGDPYBYYYY.XLSX, where YYYY is the production year.
#' @return a data frame containing the data
#'
#' @examples
#'
#'  dd <- read_mva(indata)
#'  dim(dd)
#'  head(dd)
#'
#' @export
#' @author \email{valentin@@todorov.at}
#'

read_mva <- function(indata, prod_year=ProductionYear)
{
    mva <- as.data.frame(read_excel(path=file.path(indata, paste0("MVAGDPYB", prod_year, ".xlsx"))))
    head(mva)
    dim(mva)

    compvar <- if("COMPTYPE" %in% colnames(mva)) "COMPTYPE" else "CompType"
    ## Select only countries, no country groups
    mva <- mva[which(mva[, compvar] == "M"), c("acode", "desc", "year", "gdpcod", "mvacod", "gdpcud", "mvacud", "pop")]

    colnames(mva)[1] <- "country"
    colnames(mva)[2] <- "cname"
    head(mva)
    dim(mva)
    ctlist <- unique(mva$country)

    mva[,4:8] <- 1000*mva[,4:8]

    mva
}
