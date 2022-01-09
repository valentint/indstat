##################################################################
##
## Read the ISIC R3, 2-digits, ISIC R3 and R4 files from Excel and write into .rda
##  in the work directory
##
createISIC <- function()
{
    require(readxl)
    isic32 <- read.csv(file="isic32.csv")
    isic42 <- read.csv(file="isic42.csv")
    isic3x <- read_excel("ISIC-R3.xls", sheet=1, col_types=c("text", "text"))
    isic4x <- read_excel("ISIC-R4.xls", sheet=1, col_types=c("text", "text"))
    save(isic32, isic42, isic3x, isic4x, file="isic.rda", version=2)
}
