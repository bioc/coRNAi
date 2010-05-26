\name{interactiontable}
\alias{interactiontable}
\title{
Returns a list of interactions with associated statistics. 
}
\description{
This is a extanded wrapper around the topTable function from the limma
package, as an option the ordinary t statistics can be calculated as
well. 
}
\usage{
interactiontable(ebfit, sort = "none", ord.t = FALSE, correction = "BH")
}

\arguments{
  \item{ebfit}{
    \code{ebfit} a MArrayLM object produced by the eBayes function
}
  \item{sort}{
    character string specifying which statistic to rank genes by,
    possible arguments are none, ID,size,
  t,B,adj.P.val,P.Value, and if ord.t = TRUE: ord.t, ord.p
  and ord.p.adj. 
}
  \item{ord.t}{
    Logical, should ordinary t statistics be calculted? Default is
  FALSE. 
}
  \item{correction}{
    method used to adjust the p-values for multiple testing.
          Default is BH. See \code{p.adjust} for the complete
          list of options. 

}
}

\value{
  Returns a dataframe where the rows are the interaction pairs and the
  columns the statistics:

  ID: Interaction pair if

  size: the average interaction size

  t: the moderated t statistics

  P.Value: p-value for the moderated t statistics

  adj.P.Val: adjusted p-value

  B: the b statistics

  if the ord.t=TRUE, the ordinary t statistics (ord.t), with
  correspnding p-values (ord.p) and adjusted p-values (ord.p.adj) 
 
}
\author{
Elin Axelsson
}

\section{Warning }{
usage of the ordinary t statistics is not recommended for data sets with
few replicates.
} 

\seealso{
  \code{\link{p.adjust}},\code{\link{topTable}}
}
\examples{

## simulated data
     y <- matrix(rnorm(50*4,sd=1),50,4)
     rownames(y) <- paste("Pair",1:50)
    
     # fit and eBayes
     fit <- lmFit(y)
     fit <- eBayes(fit)
     tt = interactiontable(fit,sort="size")
     head(tt)
}

