BetweenScreenPlot = function(df,what="value",names,smooth=TRUE){
  #df = df[df$weight>0,]
	rL = split(df[,what],df$replicate)
	if(missing(names)) names = paste("replicate",1:length(rL),sep="")
  if(length(names)!=length(rL))
    stop("wrong length of names argument")
  names(rL) = names
        if (smooth==TRUE){
          pairs(rL,lower.panel=panel.corSper,upper.panel=panel.smoothScatter)
        }
        else{
          pairs(rL,lower.panel=panel.corSper,upper.panel=panel.line)
        }
}
