\name{df2fitmatrix}
\alias{df2fitmatrix}

\title{
matrix for lmFit from dataframe
}
\description{
converts a dataframe into the right format for lmFit
  function
}
\usage{
df2fitmatrix(df)
}

\arguments{
 \item{df}{dataframe from cellHTS2df function}
}


\value{
  A matrix with the genepairs as rows and the replicates as
 columns. This matrix is in the right format for the \code{lmFit}.
}

\author{
Elin Axelsson 
}



\seealso{
\code{\link{df2lmFit}}
}
\examples{


data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral="Fluc")
df = weightDf(df)
lmm = lmmain(df)
df = updateDf(df,lmm)
mfit = df2fitmatrix(df)
head(mfit)
}