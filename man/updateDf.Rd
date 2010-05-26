\name{updateDf}
\alias{updateDf}

\title{
updates dataframe after fitting of main estimates
}
\description{
adds residuals (value-main effects) to a dataframe
}
\usage{
updateDf(df, lm, per=NULL)
}

\arguments{
  \item{df}{
    \code{df} dataframe created by cellHTS2df function 
}
  \item{lm}{
    \code{lm} lm objects, residuals from fitting main effects to data 
  }
  \item{per}{
    \code{per} string argument, for which factor the analysis was
  done separetly, eg. replicate. 
}
}

\value{
data frame with a new column "residuals"
}

\author{
Elin Axelsson 
}





\examples{
data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral="Fluc")
df = weightDf(df,exclude=c("double","controlP1","controlP2","controlN1","controlN2","controlP1N1"))
lmain =lmmain(df)
df = updateDf(df,lmain)
hist(df$residuals)
}

