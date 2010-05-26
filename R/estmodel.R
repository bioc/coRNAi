estmodel=function(df,estimate =c("median","mean","shorth"),per=NULL){
  if(length(estimate)>1)
    stop("wrong estimates argument, please use median, mean or short")
  if(!"weight"%in%colnames(df))
    stop("please add weight column in df to indicate which data points to include in the analysis. See function 'weightDf' for more info")
  if(!is.null(per)){
    df = split(df,df[,per])
    res = lapply(df,estpart,estimate)
  } else res = estpart(df,estimate)
  
  res
}
  

 
