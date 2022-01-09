######
##  VT::26.08.2019
##
##
##  roxygen2::roxygenise("C:/projects/statproj/R/yearbook2019")
##
##  CMYK to RGB color conversion:
##  https://www.rapidtables.com/convert/color/cmyk-to-rgb.html
##
showcolors <- function(pal, n, colormodel=c("srgb", "cmyk", "cmyk1", "cmyk1a", "cmyk2", "cmyk2a", "cmyk3"))
{
    colormodel <- match.arg(colormodel)

    if(missing(pal))
        pal <- getcolors(n, colormodel)
    else if(missing(n))
        n <- length(pal)

    rgb <- col2rgb(pal)
    rgbcol <- c("red", "green", "blue")
    image(1:n, 1, as.matrix(1:n), col=pal)
    mtext(pal, 3, 0, at=1:n)
    for(i in 1:nrow(rgb))
        mtext(rgb[i,], 3, 4-i, at=1:n, col=rgbcol[i])
    mtext("R", 3, 3, at=0.5, col=rgbcol[1])
    mtext("G", 3, 2, at=0.5, col=rgbcol[2])
    mtext("B", 3, 1, at=0.5, col=rgbcol[3])
}

#' Convert a CMYK color to RGB using the simplest conversion method
#'
#' @param cmyk A vector of length 4 containing the color-specification in CMYK. Each element must
#' be between 0 and 100 (i.e., shares in percent)
#' @param output Should the resulting RGB color be returned as a hexadecimal string (e.g., #12F212)
#' or as a vector of length 3 where each element is between 0 and 255.
#'
#' @examples
#' ## Examples (see colormodel cmyk1 below
#' cmyk2rgb(c(97,94,0,75))
#' ##  [1] "#020440"
#'
#' round(cmyk2rgb(c(97,94,0,75), output="rgb"))
#' ## r  g  b
#' ## 2  4 64
#'
#' ## Please note two different CMYK colors return the same RGB:
#' cmyk2rgb(c(0,87,84,7))
#' ##  [1] "#ED1F26"
#' cmyk2rgb(c(7,88,85,0))
#' ##  [1] "#ED1F26"
#'
#' rgb2cmyk("#020440")
#' ## c  m  y  k
#' ##97 94  0 75
#'
#' rgb2cmyk(c(2, 4, 64))
#' ## c  m  y  k
#' ##97 94  0 75
#'
#' for(i in 1:5)
#'     print(rgb2cmyk(getcolors(5, colormodel="cmyk1")[i]))
#'
#'

cmyk2rgb <- function(cmyk, output = c("hex", "rgb")) {
    if (length(cmyk) != 4 && !is.numeric(cmyk)) {
        stop("`cmyk` must be a numeric vector with exactly 4 entries")
    }

    if(any(cmyk > 100 | cmyk < 0)) {
        stop("`cmyk` values must be between 0 and 100");
    }

    output <- match.arg(output);

    if(is.null(names(cmyk))) {
        names(cmyk) <- c("c", "m", "y", "k");
    }

    cmyk <- cmyk/100

    rgb <- c(
        r = unname(255 * (1 - cmyk["c"]) * (1 - cmyk["k"])),
        g = unname(255 * (1 - cmyk["m"]) * (1 - cmyk["k"])),
        b = unname(255 * (1 - cmyk["y"]) * (1 - cmyk["k"]))
    );

    return(switch(output,
        rgb = rgb,
        hex = sprintf("#%02X%02X%02X", round(rgb["r"]), round(rgb["g"]), round(rgb["b"]))))
}

#' Convert an RGB color to CMYK using the simplest conversion method
#'
#' @param rgb A vector of length 3 containing the color-specification in RGB (each element must
#' be between 0 and 255) or a character hex color specification

rgb2cmyk <- function(rgb) {

    if(missing(rgb))
        stop("Please provide an RGB color code.")

    if(is.character(rgb))
        rgb <- col2rgb(rgb)

    if(length(rgb) != 3 && !is.numeric(rgb)) {
        stop("`rgb` must be a numeric vector with exactly 3 entries or a character containing a hexadecimal color")
    }

    cmyk <- rep(NA, 4)
    names(cmyk) <- c("c", "m", "y", "k");

    rgbx <- rgb/255
    K <- cmyk[4] <- 1-max(rgbx)
    C <- cmyk[1] <- (1 - rgbx[1] - K) / (1 - K)
    M <- cmyk[2] <- (1 - rgbx[2] - K) / (1 - K)
    Y <- cmyk[3] <- (1 - rgbx[3] - K) / (1 - K)

    return(round(100*cmyk))
}

rgb2hex <- function(r,g,b)
    rgb(r, g, b, maxColorValue = 255)

getcolors <- function(n=1, colormodel=c("srgb", "cmyk", "cmyk1", "cmyk1a", "cmyk2", "cmyk2a", "cmyk3"))
{

colormodel <- match.arg(colormodel)

if(colormodel == "srgb")
{
    switch(n, rgb(68, 119, 170, maxColorValue = 255),
        rgb(c(68, 204), c(119, 102), c(170, 119), maxColorValue = 255),
        rgb(c(68, 221, 204), c(119, 204, 102), c(170, 119, 119), maxColorValue = 255),
        rgb(c(68, 17, 221, 204), c(119, 119, 204, 102), c(170, 51, 119, 119), maxColorValue = 255),
        rgb(c(51, 136, 17, 221, 204), c(34, 204, 119, 204, 102), c(136, 238, 51, 119, 119), maxColorValue = 255),
        rgb(c(51, 136, 17, 221, 204, 170), c(34, 204, 119, 204, 102, 68), c(136, 238, 51, 119, 119, 153), maxColorValue = 255),
        rgb(c(51, 136, 68, 17, 221, 204, 170), c(34, 204, 170, 119, 204, 102, 68), c(136, 238, 153, 51, 119, 119, 153), maxColorValue = 255),
        rgb(c(51, 136, 68, 17, 153, 221, 204, 170), c(34, 204, 170, 119, 153, 204, 102, 68), c(136, 238, 153, 51, 51, 119, 119, 153), maxColorValue = 255),
        rgb(c(51, 136, 68, 17, 153, 221, 204, 136, 170), c(34, 204, 170, 119, 153, 204, 102, 34, 68), c(136, 238, 153, 51, 51, 119, 119, 85, 153), maxColorValue = 255),
        rgb(c(51, 136, 68,
            17, 153, 221, 102, 204, 136, 170), c(34, 204, 170,
            119, 153, 204, 17, 102, 34, 68), c(136, 238, 153,
            51, 51, 119, 0, 119, 85, 153), maxColorValue = 255),
        rgb(c(51, 102, 136, 68, 17, 153, 221, 102, 204, 136,
            170), c(34, 153, 204, 170, 119, 153, 204, 17, 102,
            34, 68), c(136, 204, 238, 153, 51, 51, 119, 0, 119,
            85, 153), maxColorValue = 255), rgb(c(51, 102, 136,
            68, 17, 153, 221, 102, 204, 170, 136, 170), c(34,
            153, 204, 170, 119, 153, 204, 17, 102, 68, 34, 68),
            c(136, 204, 238, 153, 51, 51, 119, 0, 119, 102, 85,
                153), maxColorValue = 255))
} else if(colormodel=="cmyk" | colormodel=="cmyk1")              # Supplement to Yearbook 2019
{
    ##  CMYK 98,28,0,25  RGB 4,138,191  HEX: #048ABF
    ##  CMYK 94,17,0,5   RGB 15,201,242 HEX: #0FC9F2
    ##  CMYK 98,0,36,25  RGB 4,191,122  HEX: #04BF7A
    ##  CMYK 0,14,84,5   RGB 242,208,39 HEX: #F2D027
    ##  CMYK 0,100,61,0  RGB 255,0,100  HEX: #FF0064
    cmyk <- rgb(c(4, 15, 4, 242, 255), c(138, 201, 191, 208, 0), c(191, 242, 122, 39, 100), maxColorValue = 255)
    cmyk[1:n]
} else if(colormodel=="cmyk1a")                                  # Supplement to Yearbook 2019 - gradient colors, for maps
{
    ##  CMYK 98,28,0,25  RGB 4,138,191   HEX: #048ABF
    ##  CMYK 95,27,0,5   RGB 12,177,242  HEX: #0CB1F2
    ##  CMYK 94,17,0,5   RGB 15,201,242  HEX: #0FC9F2
    ##  CMYK 89,9,0,5    RGB 27,220,242  HEX: #1BDCF2
    cmyk <- rgb(c(4, 12, 15, 27), c(138, 177, 201, 220), c(191, 242, 242, 242), maxColorValue = 255)

    ##  CMYK 78,31,0,55  RGB 25,79,115   HEX: #194F73
    ##  CMYK 79,32,0,11  RGB 47,154,226  HEX: #2F9AE2
    ##  CMYK 42,17,0,9   RGB 134,193,233  HEX: #86C1E9
    ##  CMYK 11,4,0,4    RGB 217,234,244  HEX: #D9EAF4
    cmyk <- rgb(c(25, 47, 134, 217), c(79, 154, 193, 234), c(115, 226, 233, 244), maxColorValue = 255)
    cmyk[1:n]
} else if(colormodel=="cmyk2")                                   # Supplement to Yearbook 2019
{
    ##  CMYK 97,94,0,75  RGB 2,4,64      HEX: #020440
    ##  CMYK 20,0,6,15   RGB 173,217,204 HEX: #ADD9CC
    ##  CMYK 0,19,78,5   RGB 242,196,53  HEX: #F2C435
    ##  CMYK 0,62,81,0   RGB 255,97,48   HEX: #FF6130
    ##  CMYK 7,88,85,0   RGB 237,31,38   HEX: #ED1F26
    ##       0,87,84,7 -->  RGB 237,31,38   HEX: #ED1F26
    cmyk <- rgb(c(2, 173, 242, 255, 237), c(4, 217, 196, 97, 31), c(64, 204, 53, 48, 38), maxColorValue = 255)
    cmyk[1:n]
} else if(colormodel=="cmyk2a")                                  # Gradient colors, for maps
{
    ##  RGB 255, 212,120
    ##  RGB 242, 195, 53
    ##  RGB 204, 150, 35
    ##  RGB 128,106, 60
    cmyk <- rgb(c(255, 242, 204, 128), c(212, 195, 150, 106), c(120, 53, 35, 60), maxColorValue = 255)
    cmyk[1:n]
} else if(colormodel=="cmyk3")                                   # Saudi Arabia competitiveness report
{
    ##  RGB 242,  22,  40
    ##  RGB 242, 163,  15
    ##  RGB 242,  92,   5
    ##  RGB 166, 135, 118
    ##  RGB  89,   5,   5
    cmyk <- rgb(c(242, 242, 242, 166, 89), c(22, 163, 92, 135, 5), c(40, 15, 5, 118, 5), maxColorValue = 255)
    cmyk[1:n]
}

}
