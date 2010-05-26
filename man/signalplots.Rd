\name{signalplots}
\alias{signalplots}
\alias{SDplot}
\alias{MAplot}

\title{
plot variation vs signal intensity
}
\description{
plots the variation of replicates vs the mean intensity either by within
screen replicate separatly or over all screen replicates.
}
\usage{
SDplot(df, xlab="intensity mean",ylab="sd",add=FALSE,main,...)
MAplot(df, main,rank=FALSE)
}

\arguments{
  \item{df}{
    \code{df} dataframe created by cellHTS2df function
  }
  \item{main}{
    \code{main} character string to be used as main title 
  }
  \item{xlab}{
    \code{xlab} label for x-axis
  }
  \item{ylab}{
    \code{ylab} label for y-axis
  }
  \item{add}{
    \code{add} logical, should result be added to existing plot
    }
  \item{rank}{
    \code{rank} if TRUE the rank of the average intensities will be used
  }
  \item{...}{
    \code{...} further arguments to be passed to the plot function.
    }
  }

\value{
  plot 
}

\author{
Elin Axelsson 
}



\seealso{
  \code{\link{plot}}
}
\examples{
data(screen1_raw)
df=cellHTS2df(screen1_raw,neutral="Fluc")
MAplot(df,main="raw data")
SDplot(df,main="raw data")
}

