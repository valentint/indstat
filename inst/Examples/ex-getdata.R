years=c(1990, 2000, 2010, 2020)
cgrps=c("DEV", "LDC", "LDCAF", "LDCAS")

## Select constant data
GDP <- select.gdp(years, cgrps)
MVA <- select.mva(years, cgrps)

## Select constant data with the general function select.direc()
GDP1 <- select.direct(years, cgrps, var="GDPCOD")
MVA1 <- select.direct(years, cgrps, var="MVACOD")       #default

all.equal(GDP, GDP1)
all.equal(MVA, MVA1)

## Select current data with the general function select.direc()
GDPCUD <- select.direct(years, cgrps, var="GDPCUD")
MVACUD <- select.direct(years, cgrps, var="MVACUD")
