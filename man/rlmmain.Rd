\name{LS main}
\alias{rlmmain}
\alias{lmmain}
\title{main effect estimation }
\description{
for rlmmain the main effects are estimated using rlm function from MASS
package, with lmmain the OLS is used.
}
\usage{
rlmmain(df,per=NULL)
lmmain(df,per=NULL)
}
\arguments{
  \item{df}{
 \code{df} dataframe created by function cellHTS2df 
}
\item{per}{
  \code{} string argument for which factor the analysis should be done
separately, eg. replicate or batch
}
}


\value{
lm,rlm
}

\author{
Elin Axelsson 
}




\seealso{
  \code{\link{rlm}},\code{\link{lm}} 
}
\examples{
data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral="Fluc")
df = weightDf(df,exclude=c("controlP1","controlP2","controlN1","controlN2","controlP1N1","double"))
mains = rlmmain(df)
hist(coef(mains))
}


