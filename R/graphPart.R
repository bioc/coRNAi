graphPart=function(ms,key,file,colors){
  ms[upper.tri(ms)]=0
  ms[is.na(ms)]=0
  anon = ms
  dimnames(anon)=NULL
  interactions = as.data.frame(rbind(which(anon>0,arr.ind=T),which(anon<0,arr.ind=T)))
  rownames(interactions)=NULL
  value = 5*c(ms[ms>0],abs(ms[ms<0])) 
  interactions$row = rownames(ms)[interactions$row]
  interactions$col = rownames(ms)[interactions$col]
  incNodes = (union(interactions$col,interactions$row))
  nodeNames = rownames(ms)
  if(class(key)=="data.frame"){
    cc_genes = with(key, GeneID[intT==1])
    isCC=nodeNames%in%cc_genes
  }
  ispos = c(rep(TRUE,length(which(ms>0))),rep(FALSE,length(which(ms<0))))
  
  
  
  out = c("graph {",
    "graph [size=\"15,9.27\" ratio=0.618034 mode=major outputorder=edgesfirst];",
    "graph [overlap=false splines=true];")
  
  out = c(out, paste("\"", interactions[,"row"], "\" -- \"", interactions[, "col"],"\"",
    sprintf(" [penwidth=%f color=%s];",
            value,
            ifelse(ispos,colors$pos,colors$neg)), sep=""))
  if(class(key)=="data.frame"){
    out = c(out, paste("\"", nodeNames, "\"",
      sprintf(" [shape=ellipse style=filled color=%s];",ifelse(isCC,colors$key,colors$node)),sep=""))
  }
  else{
    out = c(out, paste("\"", incNodes, "\"",
      sprintf(" [shape=ellipse style=filled color=%s];",colors$node),sep=""))
  }
  out = c(out, "}")

  outname =  paste(file,".dot",sep="")
  writeLines(out, con=outname)
  
  for(alg in c("neato", "fdp")) {
    svgfile =sprintf("%s-%s.pdf",file,alg)
    system(sprintf("%s %s -Tpdf  -o%s", alg,outname, svgfile))
  }
}
