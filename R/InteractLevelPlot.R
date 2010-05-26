InteractLevelPlot=function(toptable,thresh=0.001,by="P.Value",key=FALSE,col.regions = colorRampPalette(c("blue", "white", "yellow")),zerolimit=0){
  #if(class(key)=="data.frame")
  if(!missing(key))
                                        #cc_genes = with(key, GeneID[intT==1])
    cc_genes = names(key)[key==1]
    toptable$interaction=ifelse(toptable[,by]<thresh,toptable$size,0)
  m = tt2matrix(toptable,what="interaction")
  if(!missing(key)){
    roword = order( rownames(m) %in% cc_genes, decreasing=TRUE )
    colord = order( colnames(m) %in% cc_genes, decreasing=TRUE )
    m = m[ roword, colord ]
  }
  myb = seq(-(max(abs(m),na.rm=TRUE)),-zerolimit,by=0.1)
  mybreaks = c(myb,-1*myb[length(myb):1])
  plot(levelplot(m,col.regions = col.regions,scales = list(x = list(rot=90)),at = mybreaks,ylab="",xlab=""))
}
