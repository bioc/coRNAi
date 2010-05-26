\name{repplots}
\Rdversion{1.1}
\alias{repplots}
\alias{BetweenScreenPlot}
\alias{WithinScreenPlot}
\title{reproducibility plots}

\description{plots reproducibility of replicates within/between screens}
\usage{
BetweenScreenPlot(df, what="value",names,smooth=TRUE)
WithinScreenPlot(df, what="value",main="within-screen replicates",ylab ="technical replicate 2",xlab= "technical replicate 1",smooth=TRUE,...)
}

\arguments{
  \item{df}{
    \code{df} dataframe created by the cellHTS2df function
}
  \item{names}{
	\code{names} optional, character vector with names of the different
screens.
}	
\item{what}{
  \code{what} what should be plotted, eg value or residuals
  }
 \item{main}{
   \code{main} main title
 }
\item{ylab}{	
  \code{ylab} label for y-axis
}
\item{xlab}{	
  \code{xlab} label for x-axis
}
\item{smooth}{
\code{smooth} shoud the smoothScatter function be called. Default is TRUE.
}	
 \item{...}{
    \code{...} further argument to be passed to the plot function
    }
}

\value{
  pairs plot
}

\author{
Elin Axelsson 
}


\seealso{
 \code{\link{pairs}},\code{\link{plot}}
}
\examples{
data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral="Fluc")
BetweenScreenPlot(df)
WithinScreenPlot(df)
}
\keyword{hplot}
