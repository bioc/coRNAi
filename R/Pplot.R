Pplot = function(x, col="darkblue", maintitle="", nrpoints=100, ...){
  ps   = sort(1-x)
  isel = seq(1, length(x), length=nrpoints)
  plot(NA, xlim=c(0,1), ylim=c(0, length(ps)) / 1000, type="n",
       ylab=expression( paste( N(p[i]), " / 1000" ) ),
       xlab=expression(1-p[i]),
       main = maintitle,
       cex.axis = 1, las = 1)
  ifit = as.integer(round(length(ps)/3))
  ifit = min(which(ps>0.5))
  slope = ifit / ps[ifit] 
  abline(a=0, b = slope / 1000, lty=2, lwd=1)
  points(ps[isel], isel / 1000, cex = .5, pch = 19,col =col) # Defined above
}
