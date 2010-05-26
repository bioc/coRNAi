array2Larray=function(array){
  na = dimnames(array)
  tarray=array(NA,dim(array))
  for (i in 1:dim(array)[3])
    tarray[,,i] = t(array[,,i])
  totar = array(c(array,tarray),c(dim(array)[1:2],2*dim(array)[3]))
  #for (i in 1:dim(totar)[3])
   # totar[,,i][lower.tri(totar[,,i])]=NA

  dimnames(totar)=list(na[[1]],na[[2]])
  totar
}
