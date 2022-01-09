##################################################################
##
##
create_rcodes <- function()
{
    library(indstat)

    rcodes[which(rcodes$ACODE==531), "ISOCN"] <- "CUW"
    raggr[which(raggr$ACODE==531), "ISOCN"] <- "CUW"
    raggr[which(raggr$CCODE==531), "ISOCN"] <- "CUW"

    save(rcodes, raggr, file="country_codes.rda", version=2)
}
