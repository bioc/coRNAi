MainFitPlot = function(fit,xlab="Fitted values",ylab="Residuals",sd.fit=TRUE,main="Residuals vs Fitted",...){
  if(length(intersect(class(fit),"list"))>0){
    y = unlist(lapply(fit,function(x) y = x$residuals))
    x = unlist(lapply(fit,function(x) y = x$fitted.value))
  } else{
    y = fit$residuals
    x = fit$fitted.value}
  plot(x,y,type="n",xlab=xlab,ylab=ylab,main=main)
  panel.smooth(x,y,lwd=2,...)
  abline(h = 0, lty = 3, col = "gray")
  if (sd.fit){
    lfit = locfit(y~x)
    w = lfknots(lfit,what ="x")
    n = predict(lfit,what="var")
    fitv = locfit(sqrt(n)~w)
    abline(h = 0, lty = 3, col = "gray",lwd=2)
    lines(fitv,col="blue",lwd=2)
  }
}

