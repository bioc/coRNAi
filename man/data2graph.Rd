\name{data2graph}
\alias{data2graph}

\title{
Function to create .dot files for graph representation of data
}
\description{
From a interaction table or list of data matrices a .dot file is created
for visualisation of the interaction/correlation network
}
\usage{
data2graph(indata, sizethres=0, thres, thresBy = "P.Value", cols = c("blue", "white", "red"), gamma.col = 1, scaleFactor = 1, nodecolor = "white", writedot = FALSE, filename, width=0.1, penwidth=1, shape='circle', fixedsize=TRUE,fontsize=10)
}

\arguments{
  \item{indata}{
    
  }
  \item{sizethres}{
    numerical, lower treshold on the absolute effect size for edges
  }
  \item{thres}{
    threshold that should be used for interactions/correlations to be
    included in graph
  }
  \item{thresBy}{
    what data should the the threshold by used at. By default the p
    value from the moderated t test is used but one could also use
    e.g. the ordinary t or the size
  }
  
  \item{cols}{
    colors to be used in the plot, should be a character vector with the
    colors for low, neutral and hig values
  }
  \item{gamma.col}{
    Factor used to scale the colors
  }
  \item{scaleFactor}{
    Scale factor to adjust the distances beteen nodes in the graph
  }
  \item{nodecolor}{
    character or character vector, which color(s) should the nodes
  have. Should either be of lenth 1 (all nodes same color) or same
  length as the number of nodes.
  }
  \item{writedot}{
    logical, should a .dot file be created.

  }
  \item{filename}{
    charcter string with name of .dot file
    
  }
  \item{width}{
    width of the nodes
  }
  \item{penwidth}{
    width of the lines in the plot
  }
  \item{shape}{
    shape of the nodes in the plot
  }
  \item{fixedsize}{
    should all nodes have the same size
  }
  \item{fontsize}{
    size of the font in the plot
  }
}

\value{
  a .dot file is written if writefile argument is TRUE 
  
  \item{ninf}{Dataframe with information about the nodes in the network }
  \item{einfo}{Dataframe with information about the edges in the network}
  
}

\author{
  Greg Pau, Elin Axelsson 
}

\seealso{
  Also see Graphviz documantation http://www.graphviz.org/
}
\examples{#see vignette}