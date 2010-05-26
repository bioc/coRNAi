cellHTS2df = function(x,neutral){
  dataa = fData(x)
  if (length(setdiff(c("well", "plate", "ID1", "ID2" ,"Type"),colnames(dataa))>0)){
    print(setdiff(c("well", "plate", "ID1", "ID2," ,"Type"),colnames(dataa)))
    stop("missing mandatory column")
        }
  datam = Data(x)
  nrRep = dim(datam)[2]
  if(nrRep>1){
    tot = apply(datam,2,function(x) y = data.frame(dataa,x))
    df = do.call("rbind",tot)
  } else df = data.frame(dataa,datam)
  value=df[,ncol(df)]
  df = df[,-(ncol(df))]
  df$value=value
  if(nrRep>1)
    df$replicate = matrix(sapply(1:nrRep,function(x) rep(x,dim(dataa)[1])),ncol=1)
  df$Pair =  paste(mapply(max,as.character(df$ID1),as.character(df$ID2)),mapply(min,as.character(df$ID1),as.character(df$ID2)),sep=" ")
  df$Direction = mapply(function(x,y) ifelse(x==y,1,2),as.character(df$Pair),as.character(df$GeneID))
  attr(df,"neutral") = neutral
  df
}



