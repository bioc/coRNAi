ROCstat = function(trueNegSet,truePosSet,testSet,thrs,stats=c("ord.p","mod.p","size"),lft=0){
  if (stats=="ord.p"){
    negs  = testSet[testSet$ord.p<thrs & testSet$size<lft,]
    pos = testSet[testSet$ord.p<thrs & testSet$size>lft,]
  }
  if (stats=="mod.p"){
    negs  = testSet[testSet$P.Value<thrs & testSet$size<lft,]
    pos = testSet[testSet$P.Value<thrs & testSet$size>lft,]
  }
 
  if (stats=="size"){
    negs  = testSet[testSet$size<(-1*thrs),]
    pos = testSet[testSet$size>(1*thrs),]
  }
  true.positive.hits =  length(intersect(rownames(pos),rownames(truePosSet)))+length(intersect(rownames(negs),rownames(trueNegSet)))
    
  false.positive.hits = length(setdiff(rownames(negs),rownames(trueNegSet)))+ length(setdiff(rownames(pos),rownames(truePosSet)))   
    
  false.negative.hits = length(setdiff(rownames(trueNegSet),rownames(negs)))+ length(setdiff(rownames(truePosSet),rownames(pos)))
    
  true.negative.hits = length(rownames(testSet))-false.negative.hits-false.positive.hits-true.positive.hits
    
  false.positive.hits = length(setdiff(rownames(negs),rownames(trueNegSet)))+ length(setdiff(rownames(pos),rownames(truePosSet)))   
    TPR = true.positive.hits/(length(rownames(trueNegSet))+length(rownames(truePosSet)))
     FPR = false.positive.hits/(false.positive.hits+true.negative.hits)
                                        
      
  FDR = false.positive.hits/(true.positive.hits+false.positive.hits)
  
  result = c(TPR,FPR,FDR)
  result
}
