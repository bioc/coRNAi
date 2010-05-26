MAplot = function(df,main,rank = FALSE){
  if(missing(main))
    main=""
  df = df[df$Type == "comb",]
  df1=df[df$Direction==1,]
  df2=df[df$Direction==2,]
  if(length(df2$value)!=length(df1$value))
    stop("not same number of samples in both directions")
  xlab = "average intensity"
  ylab = "replicate1-replicate2"
  if(rank)
    x = rank(0.5*(df1$value[order(df1$Pair)]+df2$value[order(df2$Pair)]))
  else
    x= 0.5*(df1$value[order(df1$Pair)]+df2$value[order(df2$Pair)])
  y = df1$value[order(df1$Pair)]-df2$value[order(df2$Pair)]
  plot(x,y,main=main,xlab=xlab,ylab=ylab)
}
