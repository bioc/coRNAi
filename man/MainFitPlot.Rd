\name{MainFitPlot}
\alias{MainFitPlot}

\title{
Diagnostic plot
}
\description{
Plots residuals vs fitted values after fitting of main effects. 
}
\usage{
MainFitPlot(fit, xlab = "Fitted values", ylab = "Residuals", sd.fit = TRUE, main = "Residuals vs Fitted", ...)
}

\arguments{
  \item{fit}{
a fit from lmmain, rlmmain or similar
}
  \item{xlab}{
label for x-axis
}
  \item{ylab}{
label for y-axis
}
  \item{sd.fit}{
logical, should the local estimator of the standard deviation be plotted
}
  \item{main}{
main title for the plot
}
  \item{\dots}{
arguments to be passed on to the plot function
}
}

\value{
a plot

}

\author{
 Elin Axelsson	
}



\seealso{
 \code{\link{locfit}}
}
\examples{

## simulated data

fitted.value = rnorm(100,2,1)
residuals = rnorm(100,0,1)
fit = list(fitted.value=fitted.value, residuals = residuals)
class(fit) = "lm"
MainFitPlot(fit)

}

\keyword{ hplot }

