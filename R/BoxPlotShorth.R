BoxPlotShorth = function(formula,data=NULL,...){
  if(missing(formula) || (length(formula) != 3))
    stop("formula missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if(is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  boxplot.info= boxplot(split(mf[[response]], mf[-response]),plot=FALSE)
  x = split(mf[[response]], mf[-response])
  shorths = sapply(x,shorth) 
  boxplot.info$stats[3,]= shorths
  bxp(boxplot.info,boxcol="black",...)
}
