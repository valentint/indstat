\name{getDerivedClass}
\alias{getDerivedClass}
\title{
    Industry classifications for data analysis derived from ISIC rev 3
}
\description{
    Returns a derived classification as a list of ISIC codes
}
\usage{
getDerivedClass(classification = c("low", "medium.low", "mht", "agro", "ict", "rb", "isic2"), isic_revision=c("R4", "4", "R3", "3"))
}
\arguments{
  \item{classification}{the name of an industry classifications for data analysis derived from ISIC rev 3}
  \item{isic_revision}{ISIC revision}
}
\value{
A vector of ISIC codes}
\references{
    UNIDO (2010), Industrial Statistics: Guidelines and Methodology.
    
    Upadhyaya SH (2011) Derived classications for industrial performance indicators. Int. Statistical
    Inst.: Proc. 58th World Statistical Congress, 2011, Dublin (Session STS022).
}
\author{ Valentin Todorov \email{valentin.todorov@chello.at} 
}

\examples{
getDerivedClass("agro")
getDerivedClass("mht")
getDerivedClass("isic2")
}

