######
##  VT::17.02.2021
##
##  roxygen2::roxygenise("C:/projects/statproj/R/yearbook2021")
##
#' @name extendMVA
#' @title Compute extended indicators based on the MVA database: GDPpc, MVApc, MVAsh, GDPgr, MVAgr, ImWMVA
#'
#' @description Computes extended indicators based on the MVA database:
#'      GDPpc, MVApc, MVAsh, GDPgr, MVAgr, ImWMVA
#'
#' @details The input data frame is the file MVAGDPYBNNNN, which must have the following columns:
#'  mvacod
#'  gdpcod
#'  pop
#'  acode - country (or country group) code
#'  desc - country name
#'  year - Year
#'
#' @param df A data frame containing the MVA database
#' @return a data frame with the same structure to which the new indicators are added
#' @examples
#'
#'  ## Load MVA and extend it by calculating MVApc, MVAsh, MVAgr, GDPgr, ImWMVA
#'  ##  dfmva <- as.data.frame(read_excel(file.path(indata, paste0("MVAGDPYB", ProductionYear, ".XLS"))))
#'
#'  ## Alternatively, use the data available in the package 'yearbook2021'
#'  library(yearbook2021)
#'  data(graphdata)
#'  head(graph.base)
#'  dfmva <- graph.base
#'  dfmva <- dfmva[, -which(colnames(dfmva) == "MVACAP")]     # remove the MVACAP column
#'
#'  ## Set the new column names to be compatible with the function 'extendMVA'
#'  colnames(dfmva) <- c('acode', 'desc', 'year', 'gdpcod', 'mvacod', 'gdpcud', 'mvacud', 'pop', 'grp')
#'  extmva <- extendMVA(dfmva)
#'  head(extmva)
#'
#' @export
#' @author Valentin Todorov, \email{v.todorov@@unido.org}

##  No aggregation is done - the country groups in the file are used. If
##  these indicators by new country groups are needed, it is necessary
##  first to aggregated and than to extend.
##
extendMVA <- function(df)
{
    df$mvapc <- round(df$mvacod/df$pop)
    df$mvash <- round(100*df$mvacod/df$gdpcod,1)

    dfx=df[which(df$acode=="040"),]
    dfw=df[which(df$acode=="WOR"),]

    .growth <- function(df)
    {
        df$gdpgr <- round(c(NA, grate(X=df$gdpcod, T=df$year, t0=df$year[1], tn=df$year[length(df$year)])$r.annual), 1)
        df$mvagr <- round(c(NA, grate(X=df$mvacod, T=df$year, t0=df$year[1], tn=df$year[length(df$year)])$r.annual), 1)
        df
    }

    .impact <- function(df, dfw)
    {
       dfx <- merge(df, dfw[,c("year", "mvacod")], by=c("year"), all.x=TRUE)
        dfx$mvaim <- round(100*dfx$mvacod.x/dfx$mvacod.y, 3)
        dfx <- merge(df, dfx[,c("year", "mvaim")], all.x=TRUE)
        dfx <- dfx[, c(2:3, 1, 4:ncol(dfx))]
    }

    df1 <- ddply(df, .variables=c("country", "cname"), .fun=.growth)
    df2 <- ddply(df1, .variables=c("country", "cname"), .fun=.impact, dfw)
    df2
}
