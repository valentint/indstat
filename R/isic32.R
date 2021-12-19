#################################################################
##
##  getDerivedClass(): Will return the specified derived classification
##      as a list of ISIC codes (ISIC Revision 3, at 2-digit level)
##
getDerivedClass <- function(classification=c("low", "medium.low", "mht", "agro", "ict", "rb", "isic2"), isic_revision=c("R4", "4", "R3", "3"))
{
    classification <- match.arg(classification)
    isic_revision <- match.arg(isic_revision)

    ## Generate the low/medium-low/medium-high-high-tech classification
    if(isic_revision == "R3" || isic_revision == 3)
    {
        low <- c(15, 16, 17, 18, 19, 20, 21, 22, 36, 37)
        med.low <- c(23, 25, 26, 27, 28)
        mht <- c( 24, 29, 30, 31, 32, 33, 34, 35)
        agro <- c(15, 16, 17, 18, 19, 20, 21, 25)
        ict <-  c(30, 31, 32, 33)
        rb <-   c(15, 16, 17, 19, 20, 21, 23, 25, 26, 27)
        isic2 <- 15:37

    } else
    {
        low <- c(10, 11, 12, 13, 14, 15, 16, 17, 18, 31, 32)
        med.low <- c(19, 22, 23, 24, 25)
        mht <- c(20, 21, 28, 33, 26, 27, 29, 30)
        agro <- c(10, 11, 12, 13, 14, 15, 16, 17, 22)
        ict <- c(26, 27)
        rb <- c(10, 11, 12, 13, 15,  16, 17, 19, 22, 23, 24)
        isic2 <- 10:32
    }

##  AGRO - ISIC R3
##    Division 15 Manufacture of food products and beverages
##    Division 16 Manufacture of tobacco products
##    Division 17 Manufacture of textiles
##    Division 18 Manufacture of wearing apparel; dressing and dyeing of fur
##    Division 19 Tanning and dressing of leather; manufacture of luggage, handbags, saddlery,
##    harness and footwear
##    Division 20 Manufacture of wood and of wood products
##    Division 21 Manufacture of paper and paper products
##    Group 251 Manufacture of rubber products

##  ICT - ISIC R3
##  Division 30 Manufacture of office, accounting and computing machinery
##  Group 313 Manufacture of insulated wire and cable
##  Division 32 Manufacture of radio, television and communication equipment
##  Group 331 Manufacture of instruments and appliances for measuring, checking,
##      testing, navigating and other purposes

## RB - ISIC R3
##  Division 15 [10, 11] Manufacture of food products and beverages
##  Division 16 [12] Manufacture of tobacco products
##  Division 17 [13] Manufacture of textiles
##  Division 19 [15] Tanning and dressing of leather; manufacture of luggage, handbags, saddlery,
##  harness and footwear
##  Division 20 [16] Manufacture of wood and of wood products
##  Division 21 [17] Manufacture of paper and paper products
##  Division 23 [19] Manufacture of coke, refined petroleum products
##  Group 251   [22] Manufacture of rubber products
##  Division 26 [23] Manufacture of other non-metallic mineral products
##  Division 27 [24] Manufacture of basic metals

##  R3  R4
##  15  10 + 11
##  16  12
##  17  13
##  18  14
##  19  15
##  20  16
##  21  17
##  22  18
##  23  19
##  24  20 + 21
##  25  22
##  26  23
##  27  24
##  28  25
##  30F (30+32+33) 26
##  31  27
##  29  28 + 33
##  34  29
##  35  30
##  36  31 + 32
##  D   C

    ret <- switch(classification,
            low = low,
            medium.low = med.low,
            mht = mht,
            agro = agro,
            ict = ict,
            rb = rb,
            isic2 = isic2)

    if(classification == "agro")
        warning("Please have in mind that only 2-digit codes are returned: instead of 251, 25 is returned")

    if(classification == "ict")
        warning("Please have in mind that only 2-digit codes are returned: instead of 313, 31 is returned")

    ret
}

##
## Add ISIC description to a data frame with ISIC code.
##
addIsic <- function(df, isic.x="isic")
{
    data(isic32)

    stopifnot(is.data.frame(df))
    stopifnot(isic.x %in% colnames(df))
    merge(df, isic32, by.x=isic.x, by.y="code")
}

getIsic <- function(isic, isic_revision=c("R4", "4", "R3", "3"))
{
    isic_revision <- match.arg(isic_revision)

    if(isic_revision == "R3" || isic_revision == 3)
    {
        data(isic32)
        isic32[which(isic32$code %in% isic),]
    } else
    {
        print("ISIC Revision 4 is not defined yet!")
        NULL
    }
}
