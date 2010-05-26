\name{df2lmFit}
\Rdversion{1.1}
\alias{df2lmFit}

\title{lmFit from dataframe}
\description{ converts a dataframe into the right format for lmFit
  function, calls the lmFit from limma and returns the result. 
}
\usage{
df2lmFit(df)
}
\arguments{
  \item{df}{dataframe from cellHTS2df function}
}


\value{
Object of class 'MArrayLM'
}

\author{
Elin Axelsson 
}

\seealso{
  \code{\link[limma]{lmFit}}
  }
\examples{

data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral="Fluc")
df = weightDf(df)
lmm = lmmain(df)
df = updateDf(df,lmm)
mfit = df2lmFit(df)
str(mfit)

}

\keyword{ models }
