weightDf = function(df,exclude=c("double","controlN2","controlP2","controlP1N1","controlN1")){
  #if("weight"%in%colnames(df))
   # weights = df$weight else
  weights = rep(1,nrow(df))
  
  weights[df$Type%in%exclude] = 0
  df$weight = weights
  df
}
  
