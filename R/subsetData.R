subsetData=function(data,sub){
  ps = sample(unique(c(data$ID1,data$ID2)),sub)
  print(ps)
  ps = sort(ps)
  d=data[which(data$ID1%in%ps & data$ID2 %in%ps),]
  d
}


