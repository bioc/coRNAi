corrmatrix = function(toptable,thrs=0){
intmatrix = tt2matrix(toptable,what="size")
intmatrix = intmatrix*(abs(intmatrix)>thrs)
  cormatrix = cor(intmatrix,intmatrix,use = "pairwise.complete.obs")
  diag(cormatrix) = NA
cormatrix
}
