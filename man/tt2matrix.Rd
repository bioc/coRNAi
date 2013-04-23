\name{tt2matrix}
\alias{tt2matrix}

\title{
Extracting data from a toptable and format it to matrix 
}
\description{
Given an dataframe with data, typically from the
\code{interactiontable}, the gene pair data is converted to a symmetric
matrix. 
}
\usage{
tt2matrix(toptable, what)
}

\arguments{
  \item{toptable}{
a dataframe with data for the pairwise interactions. Typically from the
\code{interactiontable} function.
}
  \item{what}{
character indicating which of the columns in the dataframe should be
used in the matrix. }
}
\value{
  a symmetric matrix with the selected data for gene pair i,j in
  matrix[i,j] and matrix[j,i]
}
\author{
Elin Axelsson 
}

\examples{
## simulated data
mytoptable = data.frame("ID" = c("A B", "A C", "B C"),"size"=c(1:3),stringsAsFactors=FALSE)
rownames(mytoptable) = mytoptable$ID
mat = tt2matrix(mytoptable,what="size")

}
