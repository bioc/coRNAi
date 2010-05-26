SDplot = function(df,xlab ="intensity mean", ylab ="sd", add = FALSE, main,...){
  #df = df[df$Type=="comb",]
  dataa = df2array(df,"value")
  dataa = array2Larray(dataa)
  sds = apply(dataa,1:2,sd,na.rm=T)
  means = apply(dataa,1:2,mean,na.rm=T)
  if(!add)
    plot(means,sds,xlab=xlab,ylab=ylab,main=main,...)
  else
    points(means,sds,...)
}
