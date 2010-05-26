`lmmain` <-function(df,per=NULL){
  if(!is.null(per)){
    df = split(df,df[,per])
    res = lapply(df,lmpart)
  } else res = lmpart(df)
  
  res
}
  
