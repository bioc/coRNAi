updateDf = function(df,lm,per=NULL){
  addRes = function(df,lm){
    df$residuals[!is.na(df$value)&df$weight>0]=lm$residuals
    df
  }
  if(!is.null(per)){
    dfs = split(df,df[,per])
    for (i in 1:length(lm)){
      dfs[[i]]= addRes(dfs[[i]],lm[[i]])
    }
    df = do.call("rbind",dfs)
  } else df = addRes(df,lm)

  df = with(df,df[order(plate,replicate,well),])
  df
}

