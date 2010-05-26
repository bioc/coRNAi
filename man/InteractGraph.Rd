\name{InteractGraph}
\alias{InteractGraph}
\title{
  functions to visualize interactions as a graph
}
\description{
  visualizes significant interactions as a graph
}
\usage{

InteractGraph(toptable,thresh,sizecutoff=0,by,key=FALSE,file="interactions",colors = list(neg="blue",pos="yellow",key="brown",node="grey"))
}
\arguments{
  \item{toptable}{
    \code{toptable} table from function topTable
  }
  \item{thresh}{
    \code{thresh} numeric, threshold for significance
  }
  \item{sizecutoff}{
    \code{sizecutoff} a minimal absolute size of a interaction for it to
    be included in the graph as an edge.
  }
  \item{by}{
    \code{by} column in topTable that thresh should be applied to
  }
  \item{key}{
    \code{key} optional, data frame with groupings of the genes in the toptable 
  }
  \item{file}{
    \code{file} name of the file the results will be outputed to.
  }
  \item{colors}{
    \code{colors} list with colors to be used for pos interactions, neg
    interactions, key (nodes in key) and normal nodes.    
  }
  }
  
\value{
  pdf file with graph
}

\author{
Elin Axelsson
}


\seealso{
 \code{\link{levelplot}}
}
\examples{#see vignette

}
