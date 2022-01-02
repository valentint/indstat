
<!-- README.md is generated from README.Rmd. Please edit that file -->

# indstat

The package provides utilities, examples, data sets and other materials
related to the UNIDO INDSTAT database.

## Installation/Building from source

To install the latest stable development version from GitHub, you can
pull this repository and install it using

    ## install.packages("remotes")
    remotes::install_github("valentint/indstat")

Of course, if you have already installed `remotes`, you can skip the
first line (I have commented it out).

## Example

This is a simple example which shows you basic functions of the package.

``` r
library(indstat)
#> Loading required package: XLConnect
#> XLConnect 1.0.5 by Mirai Solutions GmbH [aut],
#>   Martin Studer [cre],
#>   The Apache Software Foundation [ctb, cph] (Apache POI),
#>   Graph Builder [ctb, cph] (Curvesapi Java library),
#>   Brett Woolridge [ctb, cph] (SparseBitSet Java library)
#> https://mirai-solutions.ch
#> https://github.com/miraisolutions/xlconnect
#> INDSTAT Utilities updated for 2021 (version 0.1)
getCountryCode("Austria")
#>   040 
#> "040"
getCountryCodeISO3("Austria")
#> [1] "AUT"
getCountryName(c(156, 100, 512))
#> [1] "China"    "Bulgaria" "Oman"
```
