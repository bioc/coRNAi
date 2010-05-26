\name{estmodel}
\alias{estmodel}

\title{
  Function to do estimate the main effects from data using median, mean
  or shorth.
}
\description{
  The function estimates the main effect i from all data with the RNIi
  against i. It can be done by median, mean or shorth.  

}
\usage{
estmodel(df, estimate = c("median", "mean", "shorth"), per = NULL)
}

\arguments{
  \item{df}{
    \code{df} data frame from cellHTS2df function with extra column weight
  (see weightDf)
}
  \item{estimate}{
    \code{estimate} median, mean or shorth, decides how the main effects
    will be estimated. 
}
  \item{per}{
    \code{per} for which factor should the analyis be done separetly,
    eg. batch or replicate. 
    
  }
}


\value{
  for per =NULL, a list with
  \item{coefficient}{the estimated main effects}
  \item{residuals}{the residual after the main effects have been
    subtracted from the observations}
  for other per, a list of lists like the once described above, one for
  each level of the factor per.
 
}

\author{
Elin Axelsson
}

\examples{
data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral="Fluc")
df$weight = as.numeric(df$Type=="comb")
main = estmodel(df,estimate="median")
str(main)
}

