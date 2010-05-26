InteractGraph=function(toptable,thresh,sizecutoff=0,by,key=FALSE,file="interactions",colors = list(neg="blue",pos="yellow",key="brown",node="grey")){
  
  toptable$interaction=ifelse(toptable[,by]<thresh,toptable$size,0)
  toptable$interaction=ifelse(abs(toptable$interaction)>sizecutoff,toptable$interaction,0)
  ms = tt2matrix(toptable,what="interaction")
  graphPart(ms = ms, key=key,file=file,colors=colors)
}
