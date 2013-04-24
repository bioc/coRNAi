interactiontable = function(ebfit,sort="none",ord.t=FALSE,correction="BH"){
  ResTable = topTable(eBayes(ebfit),n=length(ebfit@.Data[[6]]),sort.by="none",adjust.method=correction)
  a=colnames(ResTable)
  a[a=="logFC"]="size"
  colnames(ResTable) = a
  if(ord.t){
    ord.t=ebfit$coef / ebfit$stdev.unscaled / ebfit$sigma
    colnames(ord.t) = "ord.t"
    ResTable$ord.t= ord.t
    defr= ebfit@.Data[[5]]
    ordinary.p.all = pt(-(abs(ord.t)),df=defr)*2
    colnames(ordinary.p.all) = "ord.p"
    ResTable$ord.p = ordinary.p.all
    ord.p.adj =matrix(p.adjust(ResTable[,"ord.p"],method=correction))
    colnames(ord.p.adj) = "ord.p.adj"
    ResTable$ord.p.adj = ord.p.adj
  }
   if (!sort%in%c(colnames(ResTable),"none"))
    stop("invalid sort argument, use any of the following: ",c(colnames(ResTable),"none"))
  if(sort%in%colnames(ResTable)){
    if (sort%in%c("size","t"))
      oo = order(abs(ResTable[,sort]),decreasing=TRUE)
    else
      oo =order(ResTable[,sort],decreasing=FALSE)
    ResTable = ResTable[oo,]
  }
  ResTable
}
