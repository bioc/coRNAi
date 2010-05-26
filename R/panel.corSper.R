panel.corSper <- function(x, y, digits=2,cex.cor)
     {
       usr <- par("usr"); on.exit(par(usr))
       par(usr = c(0, 1, 0, 1))
       r <- abs(cor.test(x,y,na.rm=T,method="spearman")$estimate)
       txt <- format(c(r, 0.123456789), digits=digits)[1]
       txt = paste("corr",txt,sep="=")
       if(missing(cex.cor)) cex.cor = 0.8/strwidth(txt)
       text(0.5,0.5,labels=txt,cex=cex.cor)
     }
