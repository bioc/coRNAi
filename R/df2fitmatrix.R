df2fitmatrix = function(df){
  if(!"residuals"%in%colnames(df))
    stop("no residuals, need to estimate main effects and update dataframe")
  df = df[df$Type=="comb",]
  df = df[!is.na(df$residuals),]
  
  ta = table(df$Pair)
  maxrep=max(ta)
  
  m = matrix(NA_real_,
    nrow = nlevels(as.factor(df$Pair)),
    ncol = maxrep,
    dimnames = list(levels(as.factor(df$Pair)),1:maxrep))

  for (j in rownames(m))
    m[j,1:length(df$residuals[which(df$Pair==j)])]=df$residuals[which(df$Pair==j)]

  m
}
 

 
