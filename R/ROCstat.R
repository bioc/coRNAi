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
  true.positive.hits =  length(intersect(pos$ID,truePosSet$ID))+length(intersect(negs$ID,trueNegSet$ID))
    
  false.positive.hits = length(setdiff(negs$ID,trueNegSet$ID))+ length(setdiff(pos$ID,truePosSet$ID))   
    
  false.negative.hits = length(setdiff(trueNegSet$ID,negs$ID))+ length(setdiff(truePosSet$ID,pos$ID))
    
  true.negative.hits = length(testSet$ID)-false.negative.hits-false.positive.hits-true.positive.hits
    
  false.positive.hits = length(setdiff(negs$ID,trueNegSet$ID))+ length(setdiff(pos$ID,truePosSet$ID))   
    TPR = true.positive.hits/(length(trueNegSet$ID)+length(truePosSet$ID))
     FPR = false.positive.hits/(false.positive.hits+true.negative.hits)
                                        
      
  FDR = false.positive.hits/(true.positive.hits+false.positive.hits)
  
  result = c(TPR,FPR,FDR)
  result
}
