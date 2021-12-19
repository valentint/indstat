library(yearbook2019)
library(reshape2)
library(plyr)

inst.classes <- c("factor","factor","integer","factor","factor","numeric","factor","factor","integer","factor")
inst.names  <- c("ctable","country","year","isic","isiccomb","value","utable","source","lastupdated","unit")
inst32      <- read.csv(file="inst32us-2019.csv", header=FALSE, col.names=inst.names, colClasses=inst.classes, na.strings="...")

save(inst32, file="inst32.rda", version=2)

## 1. Aggregate employment and value added for world and calculate productivity

## Select employment and value added for total manufacturing 2009:2017
head(inst32)
df <- inst32[which(inst32$ctable %in% c("04", "20") & inst32$isic == "D" & inst32$year >= 2009), c(1:3, 6)]
df1 <- dcast(country+year~ctable, data=df)
df2 <- ddply(df1, .variables=c("year"), summarize, EMP=sum(`04`, na.rm=TRUE), VA=sum(`20`, na.rm=TRUE))
df2$PROD <- df2$VA/df2$EMP
EMPgr <- grate(df2$EMP, df2$year, df2$year[1], df2$year[length(df2$year)])$r.annual
VAgr <- grate(df2$VA, df2$year, df2$year[1], df2$year[length(df2$year)])$r.annual
PRODgr <- grate(df2$PROD, df2$year, df2$year[1], df2$year[length(df2$year)])$r.annual
xx <- data.frame(Employment=EMPgr, Productivity=PRODgr)
