PlotHeatmap= function(toptable,colpal = colorRampPalette(c("blue","white","yellow")),key=FALSE,margins= c(7,7),na.color="grey",breaks=  seq(-1,1,by =0.01),...){
  col=colpal(200)
  resi = tt2matrix(toptable,what="size")
  heatmap.2(resi,trace="none",col= col,margins=margins,key=key,na.color = na.color,breaks = breaks,...)
}
  
