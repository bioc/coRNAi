\name{PlotHeatmap}
\alias{PlotHeatmap}

\title{
Plot a heatmap of interactions
}
\description{
Plots a heatmap of the mean residuals for each interaction pair. 
}
\usage{
PlotHeatmap(toptable, colpal = colorRampPalette(c("blue", "white", "yellow")),
key=FALSE,margins=c(7,7),na.color="grey",breaks=seq(-1,1,by=0.01),...)
}
\arguments{
  \item{toptable}{
a data frame created by with the interaction estimates as "logFC" and
  pair id as "ID". Usually created by topTable function in limma
}
  \item{colpal}{
color palette to be used in the plot
}
\item{key}{
logical should a color key be included
}
\item{margins}{margins for plot
}
\item{na.color}{color for NA values
}
\item{breaks}{mapping data to colors in colpal
}
\item{...}{
additional arguments to be passed to heatmap.2 call
}


}
\value{
a plot
}

\author{
Elin Axelsson
}

\seealso{
\code{\link{heatmap.2}}
}
\examples{

data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral="Fluc")
tt = data.frame("size"=runif(length(unique(df$Pair[df$Type=="comb"])),-2,2),stringsAsFactors=FALSE)
rownames(tt) = unique(df$Pair[df$Type=="comb"])
PlotHeatmap(tt)
}

