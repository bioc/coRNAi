`rlmmain` <- function(df,per=NULL){
  if(!"weight"%in%colnames(df))
    stop("please add weight column in df to indicate which data points to include in the analysis. See function 'weightDf' for more info")
  if(!is.null(per)){
    df = split(df,df[,per])
    res = lapply(df,rlmpart)
  } else res = rlmpart(df)
  
  res
}
  

