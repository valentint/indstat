\name{do.dotplot}
\alias{do.dotplot}
\title{
Plot one or more variables in a dataframe as a dotplot.
}
\description{
Plot one or more variables in a dataframe as a dotplot.
}
\usage{
do.dotplot(x, ctvar = "NAME", var = "AAGR", grpvar, xlab, pch, col, legend, ...)
}
\arguments{
  \item{x}{data frame}
  \item{ctvar}{name of the variable containing the labels}
  \item{var}{Name of the variable to be presented as a dotplot}
  \item{grpvar}{Grouping variable}
  \item{xlab}{Label for the X-axis}
  \item{pch}{Characters to use for the dots}
  \item{col}{Colors}
  \item{legend}{Legend}
  \item{\dots}{Other parameters to be passed to lower level functions}
}
\details{
The input data is a data frame which contains as a minimum two 
variables: one variable of type factor which will be used for 
the labels - \code{ctvar}, and one numeric variable for the values - \code{var}.

Currently only one variable can be presented as a dotplot. 

It is possible to have a grouping variable - then the dots can be presented 
by different color and symbol and a legend can be added.
}
\value{
Returns an object of class "trellis". The update method can be used to subsequently 
update components of the object, and the print method (usually called by default) 
will plot it on an appropriate plotting device.
}
%\references{}
\author{Valentin Todorov \email{valentin.todorov@chello.at}}
%\seealso{}
\examples{
## Create a data frame: use ISIC Revision 3, 2-digits and a random variable
data(isic32)
head(isic32)
df <- isic32[-which(isic32$code=="D"),]     # remove "Total Manufacturing"
df$random <- runif(nrow(df), 1, 10)
do.dotplot(df, ctvar="description", var="random")

## Change X-label, color and dot symbol
do.dotplot(df, ctvar="description", var="random", xlab="Random Variable", col="red", pch=19)

## Add grouping variable - 3 categories
df$grp <- factor(round(runif(nrow(df), 1, 3)))
head(df)
legend.text <- c("Group1", "Group2", "Group3")
do.dotplot(df, ctvar="description", var="random", grpvar="grp", xlab="Random Variable", col=getcolors(3), pch=c(17,18,19), legend=legend.text)

## Get some real data - MVA for 100=Bulgaria for 2010
data(graphdata)
x <- get.50x51(yr=2010, ct=100)
x <- merge(x, isic32, by.x = "ISIC", by.y = "code")      ## Add ISIC descriptions
head(x)
do.dotplot(x, ctvar="description", var="VALUE", xlab="MVA (thousand USD at constant 2005 prices)")

}
\keyword{ ~kwd1 }

