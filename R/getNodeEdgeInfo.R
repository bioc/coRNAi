getNodeEdgeInfo = function(mat, cols =  c("blue","white","red"),
  gamma.col=1,scaleFactor=1,nodecolor="white"){
  
  if(!isSymmetric(mat))
    stop("matrix must be symmetric")
  if(sum(colnames(mat)==rownames(mat))!=length(rownames(mat)))
    stop("colnames and rownames should be identical")
  nodes = colnames(mat)
  nums = 1:length(nodes)
  
  if(!length(nodecolor)%in%c(1,length(nums)))
    stop("wrong length of nodecolor arguments, argument should have length 1 or same length as ncol/nrow of mat")
  if(length(nodecolor)==1){
    color = rep(nodecolor,length( nums))
    names(color) = nodes}
  if(length(nodecolor)==length(nums)){
    if(!setequal(names(nodecolor),nodes))
      stop("nodecolor names must be the same as the rownames/colnames for mat")
    color = nodecolor
  }
  ninfo = data.frame(node=nums,label = nodes,stringsAsFactors=F)
  ninfo$color = color[ninfo$label]
  mat[lower.tri(mat)] = 0
  edges = which(mat!=0,arr.ind=TRUE)
  mat[is.na(mat)] = 0
  ws = mat[mat!=0]
  dotcol = ifelse(ws>0,colorRampPalette(c(cols[2],cols[3]))(101)[1+abs(ws)^gamma.col*100], colorRampPalette(c(cols[2],cols[1]))(101)[1 + abs(ws)^gamma.col * 100])
  einfo = data.frame(a =edges[,"row"],b=edges[,"col"],dist=abs(ws)*scaleFactor ,color=dotcol)

  return(list("ninfo"=ninfo,"einfo"=einfo))
 }
