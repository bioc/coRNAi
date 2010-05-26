\name{df2array}
\alias{df2array}

\title{
  function to do go from data frame to an array with values
}
\description{
  The function transforms a float column in a data frame (from cHTS2df)
  to an array.   
}
\usage{
df2array(df, what)
}

\arguments{
  \item{df}{
    Data frame (from cellHTS2df) with additional column weights.
}
  \item{what}{
    which column contains the data, eg. value or residuals
}
}

\value{
  An array with the data from the data frame.

}

\author{
  Elin Axelsson 
}


\examples{

data(screen1_raw)
df = cellHTS2df(screen1_raw,neutral = "Fluc")
df$weights = as.numeric(df$Type=="comb")

aa = df2array(df,what="value")

## see head for first replicate

head(aa[,,1])

}

