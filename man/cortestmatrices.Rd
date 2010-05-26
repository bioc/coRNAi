\name{cortestmatrices}
\alias{cortestmatrices}

\title{
Function to extract correlations and corresponding p-values from
interaction matrix.
}
\description{
This is a wrapper function for \code{cor.test}, given a matrix of
interaction values, correlations and corresponding p-values for the
genewise interaction profiles are calculated.
}
\usage{
cortestmatrices(mat, method = c("pearson", "kendall", "spearman"))
}

\arguments{
  \item{mat}{
    \code{mat} interaction matrix
  }
  \item{method}{
    character deciding which correlation method should be used
  }
}

\value{
List of two matrices
 \item{cor.matrix}{matrix with correlations}
 \item{p.matrix}{matrix with p-values}
}

\author{
Elin Axelsson 
}


\seealso{
\code{\link{cor.test}}
}
\examples{
## simulate data with 2 genes with similar profiles

mat = matrix(rnorm(100*100,0,1),100,100)
pr = sample(2:10,100,replace=TRUE)
mat[1:2,] = mat[1:2,] + matrix(pr,ncol=100,nrow=2,byrow=TRUE)
mat = mat+t(mat)
diag(mat) = NA
dimnames(mat)=list(1:100,1:100)
res = cortestmatrices(mat,method="spearman")
cors= res[[1]]
ps = res[[2]]
print(which(ps==min(ps,na.rm=TRUE),arr.ind=TRUE))

}

