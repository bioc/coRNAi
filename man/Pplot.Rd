\name{Pplot}
\alias{Pplot}

\title{
  Function to plot cumulative p-values
}
\description{
Given a vextor of p-values a cumulative p-value plot is produced
}
\usage{
Pplot(x, col = "darkblue", maintitle="", nrpoints = 100, ...)
}

\arguments{
  \item{x}{
    vector with p-values
}
  \item{col}{
    color to be used
}
  \item{maintitle}{
    character, main plot title
}
  \item{nrpoints}{
    numeric, how many points should be plotted
}
  \item{\dots}{
    additional arguments passed on to the plot 
}
}

\value{
a plot
}

\author{
Wolfgang Huber
}


\examples{
x = runif(1000,0,1)
Pplot(x,col="red",maintitle="uniform dist.", nrpoints=200)

}
