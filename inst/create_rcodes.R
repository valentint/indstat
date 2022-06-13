##################################################################
##
##
##  384=C<f4>te d'Ivoire
##  638=R<e9>union
##  531=Cura<e7>ao
##
create_rcodes <- function()
{
    library(indstat)

    rcodes[which(rcodes$ACODE==531), "ISOCN"] <- "CUW"
    raggr[which(raggr$ACODE==531), "ISOCN"] <- "CUW"
    raggr[which(raggr$CCODE==531), "ISOCN"] <- "CUW"

    rcodes$DESC <- enc2utf8(rcodes$DESC)
    raggr$ADESC <- enc2utf8(raggr$ADESC)
    raggr$CDESC <- enc2utf8(raggr$CDESC)

    rcodes[rcodes$ACODE==531, "DESC"] <- "Curaçao"
    rcodes[rcodes$ACODE==384, "DESC"] <- "Côte d'Ivoire"
    rcodes[rcodes$ACODE==638, "DESC"] <- "Réunion"

    raggr[raggr$CCODE==531, "CDESC"] <- "Curaçao"
    raggr[raggr$CCODE==384, "CDESC"] <- "Côte d'Ivoire"
    raggr[raggr$CCODE==638, "CDESC"] <- "Réunion"

    save(rcodes, raggr, file="country_codes.rda", version=2)
}
