library(yearbook2018)
library(haven)
df1 <- as.data.frame(read_sas("g:/STATISTICS/CS-YB2017/Data/MVAGDPYB2017.sas7bdat"))
df2 <- as.data.frame(read_sas("g:/STATISTICS/CS-YB2018/Data/MVAGDPYB2018.sas7bdat"))
#df1 <- read_sas("s:/UQD/YEARBOOK/2017/PrimaryData/tab1to4base.sas7bdat")
#df2 <- read_sas("s:/UQD/YEARBOOK/2018/PrimaryData/tab1to4base.sas7bdat")

##df2 <- df2[-which(df2$year==2017),]         # remove the most recent year

df1 <- df1[-which(df1$COMPTYPE!="M"),]      # remove the country groups
df2 <- df2[-which(df2$COMPTYPE!="M"),]      # remove the country groups
df1$year <- as.numeric(df1$year)
df2$year <- as.numeric(df2$year)
df1$acode <- as.character(df1$acode)
df2$acode <- as.character(df2$acode)

df <- merge(df1, df2, by=c("year", "acode", "desc", "COMPTYPE"), all=TRUE)
dim(df1)
dim(df2)
dim(df)
table(df1$Year)
table(df2$Year)

head(df)
#all.equal(df$MvaCod.x, df$MvaCod.y)
#all.equal(df$MvaCud.x, df$MvaCud.y)
#all.equal(df$GdpCod.x, df$GdpCod.y)
#all.equal(df$GdpCud.x, df$GdpCud.y)
#all.equal(df$Pop.x, df$Pop.y, tolerance=1e-1)
#all.equal(df$Pop.x, df$Pop.y, tolerance=1e-3)

df$diffpop <-  100*(df$pop.x-df$pop.y)/(df$pop.x+df$pop.y)/2
dfx <- df[which(abs(df$diffpop) > 3),]
#dfx <- addCountryName(dfx, ct.x="ACode")
summary(dfx$diffpop)
dim(dfx)

df$diff_mvacod <-  100*(df$mvacod.x-df$mvacod.y)/(df$mvacod.x+df$mvacod.y)/2
dfx <- df[which(abs(df$diff_mvacod) > 4),]
##dfx <- addCountryName(dfx, ct.x="ACode")
##dfx <- dfx[order(dfx$diff_mvacod, decreasing=TRUE),]
summary(dfx$diff_mvacod)
dim(dfx)

ct <- "076"
var <- "mvacod"
years1 <- df1[df1$acode==ct, "year"]
years2 <- df2[df2$acode==ct, "year"]
x1 <- df1[df1$acode==ct, var]
x2 <- df2[df2$acode==ct, var]
plot(years2, x2, ylim=c(min(x1,x2), max(x1,x2)), type="n")
lines(years1, x1, col="blue")
lines(years2, x2, col="red")
