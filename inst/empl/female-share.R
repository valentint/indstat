library(yearbook2015)
## data(graphdata)

library(XLConnect)

## Read the Excel file with share of female employees (TAB1.10)
wb  <- loadWorkbook("tab110base.xls")
df <- readWorksheet(wb, sheet=1)
df1 <- addCountryName(df, ct.x="CT")
df2 <- df1[,c(1, 7, 2:6)]
df2$SHARE <- round(df2$SHARE, 2)
empl <- df2
save(empl, file="empl.rda")

## Write the table with the country names appended
if(FALSE)
{
shname <- "FEM-SHARE-2015"
wb <- loadWorkbook("fem-table.xls", create = TRUE)
createSheet(wb, name=shname)
writeWorksheet(wb, df2, sheet=shname)
saveWorkbook(wb)
}

#################################################################
yr=2012

## Exclude the unnecessary ISICS
y <- df2[which(!(df2$ISIC2 %in% c("37","D"))),]

## Exclude Industrialized countries
#y <- y[which(!(y$CT %in% c("392", "410"))),]
ind <- getCountryGroup("IND")
indx <- y[which(y$CT %in% ind),]


cat("\nThe following countries will be excluded (industrialized):\n")
getCountryName(unique(indx$CT))

y <- y[which(!(y$CT %in% ind)),]


## Take only one year
y <- y[which(y$YR==yr),]

x1 <- y[,c(4,5,6)]
xaggr=aggregate(cbind(EMP, FEM) ~ ISIC2, data=x1, FUN=sum)

xaggr$shfem <- round(100*xaggr$FEM/xaggr$EMP)
xaggr$gap <- 100 - 2*xaggr$shfem
xaggr <- addIsic(xaggr, isic.x="ISIC2")
xaggr$description <- substr(xaggr$description, 1, 42)
ord <- order(xaggr$gap, decreasing=TRUE)

xaggr[ord,]
do.dotplot(xaggr, ctvar="description", var="gap", col="black", pch=23, xlab="Gender gap in manufacturing")
# do.dotplot(xaggr, ctvar="ISIC2", var="gap", col="black", pch=23)
savePlot(file="gender_gap.pdf", type="pdf")



GenderGap <- function(year=2012, filter=c("DEV"),
            type="pdf", outfile, windows=FALSE, save=FALSE, ...)
{
    data(empl)

    ## Exclude the unnecessary ISICS
    y <- empl[which(!(empl$ISIC2 %in% c("37","D"))),]

    ## Exclude Industrialized countries
##    if(only.DEV)
##    {
##        #y <- y[which(!(y$CT %in% c("392", "410"))),]
##        ind <- getCountryGroup("IND")
##        indx <- y[which(y$CT %in% ind),]
##      cat("\nThe following countries will be excluded (industrialized):\n")
##      getCountryName(unique(indx$CT))
##      y <- y[which(!(y$CT %in% ind)),]
##    }

    #y <- y[which(!(y$CT %in% c("392", "410"))),]
    ctind <- getCountryGroup(filter)
    ctsel <- y[which(y$CT %in% ctind),]
    ctrej <- y[which(!(y$CT %in% ctind)),]
    cat("\nThe following countries will be excluded (industrialized):\n")
    getCountryName(unique(ctrej$CT))
    y <- ctsel

    ## Select the year
    y <- y[which(y$YR==year),]

    ## Aggregate
    x1 <- y[,c(4,5,6)]
    xaggr=aggregate(cbind(EMP, FEM) ~ ISIC2, data=x1, FUN=sum)

    xaggr$shfem <- round(100*xaggr$FEM/xaggr$EMP)
    xaggr$gap <- 100 - 2*xaggr$shfem
    xaggr <- addIsic(xaggr, isic.x="ISIC2")
    xaggr$description <- substr(xaggr$description, 1, 42)
    ord <- order(xaggr$gap, decreasing=TRUE)

    print(xaggr[ord,])

    print(do.dotplot(xaggr, ctvar="description", var="gap", col="black", pch=23, xlab="Gender gap in manufacturing"))
    if(save)
        doSave(outfile=outfile, type=type)
}
