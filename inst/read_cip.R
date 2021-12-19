cip_year <- 2019
cip <- read.csv(file.path("G:/statistics/cip", cip_year, "ProductData", paste0("cip", cip_year, ".csv")))
head(cip)
cip$country <- padct(cip$country)

save(cip, file="cip.rda", version=2)
