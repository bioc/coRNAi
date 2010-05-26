\name{BoxPlotShorth}
\alias{BoxPlotShorth}

\title{
Boxplot with horizontal bars at the midpoint of the shorth
}
\description{
Produces a boxplot, but instead of horizontal bars at the median, the
bars are at the  midpoint of the shorth.
}
\usage{
BoxPlotShorth(formula, data = NULL, ...)
}

\arguments{
  \item{formula}{
    formula for how the boxplot should be drown.
}
  \item{data}{
    the data to be used
}
  \item{\dots}{
    other arguments to be passed to the plot function
}
}

\value{
  a boxplot object
 
}

\author{
  Elin Axelsson 
}

\seealso{
 See Also \code{\link{boxplot.formula}}
}
\examples{
data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral= "Fluc")
BoxPlotShorth(value~replicate,df)
}

