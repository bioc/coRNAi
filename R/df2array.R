df2array = function(df,what){

  controls = attr(df,"neutral")
  #if(diagelement %in% c("single","double","both"))
  diag = extractDiag(df,what=what)
  #if(nrow(diag)>0)                                      #if(class(diagelement)=="matrix")
  #diag=diagelement
  
  samples = setdiff(intersect(df$ID1,df$ID2),controls)

  dfs = df[df$Type=="comb",]
  samples = as.factor(dfs$ID2)
  dfs$ID2 = as.factor(dfs$ID2)
  dfs$ID1 = as.factor(dfs$ID1)
  dfs$replicate = as.factor(dfs$replicate)
  reps = as.factor(unique(df$replicate))
  
  resArray = array(NA_real_,
    c(rep(nlevels(samples),2),
      nlevels(reps)),
    dimnames = list(levels(samples),levels(samples),levels(reps)))
  
  resArray[cbind(as.integer(dfs$ID2),as.integer(dfs$ID1),as.integer(dfs$replicate))] = dfs[,what]
  
  if(!is.null(diag)){
    maxrep = max(dim(diag)[2],dim(resArray)[3])
    
    totArr = array(NA_real_,c(dim(resArray)[1:2],maxrep),
      dimnames = list(levels(samples),levels(samples),1:maxrep))
    totArr[,,1:dim(resArray)[3]]=resArray
    
    if(!setequal(rownames(diag),rownames(resArray)))
      stop("diagelement is not same as combinations")
    
    c= ncol(diag)
    
    diag = matrix(diag[rownames(resArray),],ncol=c)
    
    for (i in 1:dim(diag)[2])
      diag(totArr[,,i]) = diag[,i]
    resArray = totArr
  }
  
  resArray
} 
