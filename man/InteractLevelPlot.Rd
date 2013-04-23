\name{InteractLevelPlot}
\alias{InteractLevelPlot}

\title{
  function to visualize interactions as a levelplot
}
\description{
  visualizes significant interactions as a levelplot
}
\usage{
InteractLevelPlot(toptable, thresh = 0.001, by = "P.Value", key = FALSE, col.regions = colorRampPalette(c("blue", "white", "yellow")),zerolimit=0)
}
\arguments{
  \item{toptable}{
    \code{toptable} table from function topTable
  }
  \item{thresh}{
    \code{thresh} numeric, threshold for significance
  }
  \item{by}{
    \code{by} column in topTable that thresh should be applied to
  }
  \item{key}{
    \code{key} optional, data frame with groupings of the genes in the toptable 
  }
  \item{col.regions}{
    \code{col.regions} colors to be used
  }
  \item{zerolimit}{
    \code{zerolimit} threshold below which interactions should be
    colored as 0.
    }
      
}

\value{
  a levelplot, pdf files with graphs
}

\author{
Elin Axelsson
}


\seealso{
 \code{\link{levelplot}}
}
\examples{

data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral="Fluc")
tt = data.frame("size"=runif(length(unique(df$Pair[df$Type=="comb"])),-2,2),stringsAsFactors=FALSE)
rownames(tt) = unique(df$Pair[df$Type=="comb"])
InteractLevelPlot(tt,thres=0,by="size")
}
