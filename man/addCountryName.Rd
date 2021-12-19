\name{addCountryName}
\alias{addCountryName}
\alias{addCountryISO3}
\title{
Add metadata to a dataframe (country name, ISO3 code, etc)
}
\description{
Add additional information (metadata) to a data frame containing country information.
}
\usage{
addCountryName(df, ct.x = "CT", ct.y = "ACODE", ct.name = "NAME", comptype=FALSE)
addCountryISO3(df, ct.x = "CT", ct.y = "ACODE", ct.iso3 = "ISO3", comptype=FALSE)
}
\arguments{
  \item{df}{ A data frame containing country information. 
    Countries are identified by a country code, e.g. UN country code}
  \item{ct.x}{Country code field name in the data frame - defaults to \code{CT} }
  \item{ct.y}{Country code field name in the metadata data frame \code{rcodes}- defaults to \code{ACODE} }
  \item{ct.name}{Country name field name in the metadata data frame \code{rcodes}- defaults to \code{NAME} }
  \item{ct.iso3}{Country ISO3 code field name in the metadata data frame \code{rcodes}- defaults to \code{ISO3} }
  \item{comptype}{Wheather to include the component type (M=country or A=aggregate (country group)) - defaults to \code{FALSE} }
}
\value{
A data frame with the requested metadata added.
}
\references{
    UNIDO (2010), Industrial Statistics: Guidelines and Methodology.
}
\author{ Valentin Todorov \email{valentin.todorov@chello.at} }
\examples{
data("graphdata")
mvax <- get.50x51()
head(addCountryName(mvax))
head(addCountryISO3(mvax))
head(addCountryISO3(mvax, comptype=TRUE))
}

