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

    rcodes[rcodes$ACODE==531, "DESC"] <- "Cura�ao"
    rcodes[rcodes$ACODE==384, "DESC"] <- "C�te d'Ivoire"
    rcodes[rcodes$ACODE==638, "DESC"] <- "R�union"

    raggr[raggr$CCODE==531, "CDESC"] <- "Cura�ao"
    raggr[raggr$CCODE==384, "CDESC"] <- "C�te d'Ivoire"
    raggr[raggr$CCODE==638, "CDESC"] <- "R�union"

    save(rcodes, raggr, file="country_codes.rda", version=2)
}
