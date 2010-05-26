main2fit= function(df,mains){
  df=df[df$weight>0,]
  cont = attr(df,"neutral")
  mains[cont]=0
  df$m1 = mains[df$ID2]
  df$m2 = mains[df$ID1]
  res = df$value-df$m1-df$m2
  res = res[!is.na(res)]
  fit = df$m1+df$m2
  fit = fit[!is.na(fit)]
  mains = mains[!names(mains)==cont]
  res = list(coefficients=mains,residuals=res,fitted.value=fit)
  class(res) = "lm"
  res
}
