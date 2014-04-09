
whiskerplot <- function(x,parameters,quantiles=c(0.025,0.975),zeroline=TRUE){
  if(class(x)!="simplejags"){stop('Requires simplejags object as input')}
  
  n <- length(parameters)
  
  if(sum(parameters%in%names(x$means))!=n){stop('One or more specified parameters are not in model output')}
  
  xstructure <- c(1:n)
  
  means <- unlist(x$means[parameters])
  
  qs <- function(x,y){as.numeric(quantile(x,y))}
  tops <- unlist(lapply(x$sims.list[parameters],qs,quantiles[2]))
  bottoms <- unlist(lapply(x$sims.list[parameters],qs,quantiles[1]))

  ymin <- min(unlist(x$sims.list[parameters]))
  ymax <- max(unlist(x$sims.list[parameters]))
  
  plot(xstructure,means,xaxt="n",ylim=c(ymin,ymax),xlim=c(0,n+1),xlab="Parameters",ylab="Parameter Values",pch=19,cex=1.5,
       main=paste('Whisker plot, quantiles (',quantiles[1],' - ',quantiles[2],')',sep=""))
  axis(side=1, at=c(1:n), labels=parameters)
  box()
  
  if(zeroline){abline(h=0)}
  
  for (i in 1:n){
    segments(x0=xstructure[i],y0=bottoms[i],x1=xstructure[i],y1=tops[i], lwd=2)
    segments(x0=xstructure[i]-0.2,y0=bottoms[i],x1=xstructure[i]+0.2,y1=bottoms[i])
    segments(x0=xstructure[i]-0.2,y0=tops[i],x1=xstructure[i]+0.2,y1=tops[i])
  }
 
}