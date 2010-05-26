\name{cellHTS2df}
\alias{cellHTS2df}

\title{converts cellHTS objects to dataframes}
\description{converts a cellHTS2 object into a data.frame object and
  check that all mandatory meta data is included. Adds Directions,
  replicates and Pair columns.}
\usage{
cellHTS2df(x,neutral)
}

\arguments{
  \item{x}{
a cellHTS object with correct annotations
}
\item{neutral}{
  string stating which RNAi is neutral (negative control) 
}
}

\value{
data.frame, with the data from the cellHTS object in column
  "value". Meta data from annotation file and the new columns;
  Directions, Replicate and Pair
}

\author{
Elin Axelsson 
}



\seealso{
 \code{\link[cellHTS2]{cellHTS}}
}
\examples{
data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral="Fluc")
head(df)
}
