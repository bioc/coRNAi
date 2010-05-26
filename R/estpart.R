estpart=function(df,estimate =c("median","mean","shorth")){
  arr = df2array(df,"value")
  na = dimnames(arr)
  arr = array2Larray(arr)
  switch(estimate,
         median = median,
         mean = mean,
         shorth = shorth)
  
  mains = apply(arr,1,estimate,na.rm=TRUE)
  names(mains) = na[[1]]
  res = main2fit(mains=mains,df=df)
  res
}
