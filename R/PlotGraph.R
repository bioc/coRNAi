PlotGraph = function(cor.matrix, gamma.cor = 4, gamma.col = 2.5,cex=0.5,colors=c("darkred","white","darkblue"),text.col="black",text=TRUE ,plot.nodes=F,node.cex=5,node.col="black",node.fill=FALSE,...){
  coo = cor.matrix
  diag(coo) = 1
  oh <- order( abs(coo), decreasing=FALSE )
  
  fit <- isoMDS( (1-abs(coo))^gamma.cor, k=2)
  coo[coo==1]=NA
  plot( fit$points, pch=NA, axes=FALSE,ylab=NA, xlab=NA,ylim=c(min(fit$points)-0.1,max(fit$points)+0.1), xlim=c(min(fit$points)-0.1,max(fit$points)+0.1))
  
  segments( fit$points[ row(coo)[oh], 1 ], fit$points[ row(coo)[oh], 2 ], fit$points[ col(coo)[oh], 1 ], fit$points[ col(coo)[oh], 2 ], col = ifelse( coo[oh]>0, colorRampPalette(c(colors[2],colors[1]))(101)[1+abs(coo[oh])^gamma.col*100], colorRampPalette(c(colors[2],colors[3]))(101)[1+abs(coo[oh])^gamma.col*100] ),...)
  if(plot.nodes==TRUE){
    pchh =ifelse(node.fill==TRUE,19,1)
    points(fit$points[,1],fit$points[,2],cex=node.cex,col=node.col,pch=pchh)
  }
  if(text==TRUE)
    text( fit$points[,1], fit$points[,2], rownames(fit$point), col=text.col,cex=cex)
}

