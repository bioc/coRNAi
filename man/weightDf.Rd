\name{weightDf}
\alias{weightDf}

\title{
Function to indicate which data points should be involved in down stream analysis.
}
\description{
Function to do add weights to the data points in a data frame. At the
time being 0 means excluded and everything >0 means included.
}
\usage{
weightDf(df, exclude = c("double", "controlN2", "controlP2", "controlP1N1", "controlN1"))
}

\arguments{
  \item{df}{
    data frame from cellHTS2df
}
  \item{exclude}{
    which type of data should be excluded from analysis. 
}
}
\details{
See also vignette for information about different "Type" types. 
}
\value{
A data frame with an added column 'weight'
}

\author{
Elin Axelsson 
}

\examples{
data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral="Fluc")
##stupid example 
df = weightDf(df,exclude="controlN2")
head(df)
}

