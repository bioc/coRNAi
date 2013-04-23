tt2matrix=function(toptable,what){
	toptable$ID = rownames(toptable)  
	genes = sort(unique(unlist(strsplit(toptable$ID," "))))
 #genes = sort(unique(unlist(strsplit(rownames(toptable)," "))))
 if(!what%in%colnames(toptable))
    stop ("\"what\" argument not present in toptable") 
  
  dm = matrix(NA,length(genes),length(genes),dimnames = list(genes,genes))

  for (k in rownames(dm)){
    for (j in colnames(dm)){
      if(k!=j){
        ind = paste(max(k,j),min(k,j),sep= " ")
        if(!ind%in%toptable$ID)
          ind = paste(min(k,j),max(k,j),sep= " ")
        dm[k,j] = toptable[which(toptable$ID==ind) ,what]
      }
    }
  }
  dm
}
