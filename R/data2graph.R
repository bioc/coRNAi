data2graph = function(indata, sizethres=0,thres,thresBy="P.Value",cols = c("blue","white","red"),gamma.col=1,scaleFactor=1,nodecolor="white", writedot=FALSE, filename, width=0.1,penwidth=1,shape='circle',fixedsize=TRUE,fontsize=10){
  if(class(indata)=="list"){
    sizemat = indata[["size"]]
    sigmat = indata[["pvalues"]]
  }
  if(class(indata)!="list"){
    sizemat = tt2matrix(indata,what="size")
    sigmat = tt2matrix(indata,what=thresBy)
  }
  sizemat[abs(sizemat)<sizethres]=0
  sigmat = sigmat<thres
  aa=sizemat*sigmat
  diag(aa)=0
  inc = apply(abs(aa),1,sum)>0
  
  if(sum(inc)<ncol(aa)&length(nodecolor)>1){
    temp = aa[inc,inc]
    nodecolor = nodecolor[colnames(temp)]
  }
  aa = aa[inc,inc]
  
  NandE = getNodeEdgeInfo(aa,cols = cols, gamma.col=gamma.col, scaleFactor=scaleFactor,nodecolor=nodecolor)
  if(writedot){
    if(missing(filename))
      stop("please provide name of file")
    if(missing(width))
      width=0.3
    if(missing(penwidth))
      penwidth=2
    if(missing(shape))
      shape='circle'
    if(missing(fixedsize))
      fixedsize=TRUE
    writedotfile(NandE[["ninfo"]], NandE[["einfo"]], filename=filename,width=width,penwidth=penwidth,shape=shape,fixedsize=fixedsize,fontsize=fontsize)
  }
  return(NandE)
}
