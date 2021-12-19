rr <- as.numeric(as.character(rcodes$ACODE[rcodes$COMPTYPE=="M"]))
riso <- rcodes$ISOCN[rcodes$COMPTYPE=="M"]
rname <- rcodes$DESC[rcodes$COMPTYPE=="M"]
cc <- countrycode(rr, "un", "iso3c")
cn <- countrycode(rr, "un", "country.name")

df <- cbind.data.frame(rr, riso, EQ=(toupper(rname)==cn), cc, rname, cn)

head(df)
df[is.na(df$EQ),]
df[!is.na(df$EQ) & ! df$EQ,]
