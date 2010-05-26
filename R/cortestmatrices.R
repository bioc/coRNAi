cortestmatrices = function(mat,method=c("pearson", "kendall", "spearman")){
  if(is.null(colnames(mat))|is.null(rownames(mat)))
    stop("need to provide dimnames to matrix mat")
  cor = matrix(NA,ncol=ncol(mat),nrow=nrow(mat),dimnames=dimnames(mat))
  ps =  matrix(NA,ncol=ncol(mat),nrow=nrow(mat),dimnames=dimnames(mat))
  for (i in rownames(mat)){
    for (j in colnames(mat)){
      cc = cor.test(mat[i,],mat[,j],method=method)
      cor[i,j]=cc$estimate
      ps[i,j]=cc$p.value
    }
  }
  diag(cor)=NA
  diag(ps)=NA
 res = list("cor.matrix"=cor,"p.matrix"=ps)
}
         
