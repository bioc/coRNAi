lmpart = function(df){
  cont = attr(df,"neutral")
  df=df[df$weight>0,]
  mod = coRNAi:::moMatrix(df)
  mod = mod[,!colnames(mod)%in%cont]
  lmmain2 = lm(df$value~mod-1)
  names(lmmain2$coefficient)= sapply(strsplit(names(coef(lmmain2)),"mod"),"[[",2)
  lmmain2
}
