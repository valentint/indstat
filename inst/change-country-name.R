##
##  If it is necessary to change a name of a country after the Yearbook,
##  it is necessary to do it in a number of places.
##
##  Here we change the name of Macedonia in the package 'yearbook2019'
##
##

yb_year <- 2019
library(yearbook2019)
data(graphdata)
ls()

maxyear <- max(graph.base$YEAR, na.rm=TRUE)
if(maxyear == yb_year)
    graph.base <- graph.base[which(graph.base$YEAR < maxyear), ]


## Change the name of Macedonia
newname <- "North Macedonia"

graph.50x51$CN <- as.character(graph.50x51$CN)
graph.50x51[graph.50x51$CT == 807, "CN"] <- newname
graph.50x51$CN <- factor(graph.50x51$CN)

graph.base$CN <- as.character(graph.base$CN)
graph.base[graph.base$CT == 807, "CN"] <- newname
graph.base$CN <- factor(graph.base$CN)

head(rcodes)
rcodes$DESC <- as.character(rcodes$DESC)
rcodes[rcodes$ACODE==807, "DESC"] <- newname
rcodes$DESC <- factor(rcodes$DESC)

head(raggr)
raggr$ADESC <- as.character(raggr$ADESC)
raggr$CDESC <- as.character(raggr$CDESC)

raggr[raggr$ACODE==807, "ADESC"] <- newname
raggr[raggr$ACODE==807, "CDESC"] <- newname
raggr[raggr$CCODE==807, "CDESC"] <- newname

raggr$ADESC <- factor(raggr$ADESC)
raggr$CDESC <- factor(raggr$CDESC)


save("graph.50x51", "graph.base",  "graph.fem",   "graph.gdp",   "graph.mva",   "graph.pop",   "raggr", "rcodes", file="graphdata.rda", version=2)
