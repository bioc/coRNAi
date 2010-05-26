moMatrix=function(df){

  cPA = as.factor(c(df$ID1,df$ID2))
  mod = matrix(0, nrow=nrow(df),ncol=nlevels(cPA),
  dimnames= list(NULL,levels(cPA)))
  
  
  c1 = cPA[1:nrow(mod)]
  c2 = cPA[nrow(mod)+1:length(cPA)]

  mod[cbind(1:nrow(mod),as.integer(c1))]=1
  mod[cbind(1:nrow(mod),as.integer(c2))]=1

  mod
}
