extractDiag = function(df,what){

  controls = attr(df,"neutral")
  #dfnew =  switch(extract,
  #  single = df[df$Type=="controlN1",],
  #  double = df[df$Type=="double",],
  #  both = df[df$Type%in%c("controlN1","double"),])

  dfnew = df[df$Type!="comb"&df$weight>0,]

  if(sum(!unique(dfnew$Type)%in%c("double","controlN1")))
    stop("only knockdowns of type double and/or controlN1 can be included in analyisis, see function weightDf")
  
  dfnew$ID2[dfnew$ID2%in%controls] = dfnew$ID1[dfnew$ID2%in%controls]
  dfnew$ID1 = dfnew$ID2
  
  u = split(dfnew[,what],dfnew$ID2)
  res = do.call("rbind",u)
  #print(dim(res))
  res
}
