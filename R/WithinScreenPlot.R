WithinScreenPlot = function(df,what="value",main="within-screen replicates", ylab ="technical replicate 2",xlab= "technical replicate 1",smooth=TRUE,... ){
  df = df[df$Type=="comb",]
  dfx = df[df$Direction==1,]
  x = dfx[order(dfx$Pair),what]
  dfy = df[df$Direction==2,]
  y = dfy[order(dfy$Pair),what]
	if(smooth==TRUE){
          smoothScatter(x,y,xlab=xlab,ylab = ylab,main = main,asp=1,...)
	}
	else{
          plot(x,y,xlab=xlab,ylab = ylab,main = main,asp=1,...)
          abline(0,1)
	}
}
