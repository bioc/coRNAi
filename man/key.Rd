\name{key}
\alias{key}
\docType{data}
\title{
A key to data set screen1, contains (additional) information about the genes in the screen.
}
\description{
Contains information about which of the 16 genes in screen1 are cell
cycle related. This is used in interaction graphs/plots. 
}
\usage{data(key)}
\format{
  A data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{GeneID}}{a factor with levels \code{AnnIX} \code{CG12785} \code{CG16935} \code{CG3165} \code{CG7889} \code{CG8108} \code{CSN3} \code{CSN4} \code{CSN5} \code{fwd} \code{pbl} \code{Rbf} \code{Rho1} \code{sos} \code{trbl} \code{zip}}
    \item{\code{cellCycle}}{a numeric vector}
  }
}

\examples{
data(key)
table(key$cellCycle)

}
\keyword{datasets}
