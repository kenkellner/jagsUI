
whiskerplot <- function(x,parameters,quantiles=c(0.025,0.975),zeroline=TRUE){

  check_class(x)
  
  all_params <- param_names(x$samples)
  parameters <- match_params(parameters, all_params)
  if(is.null(parameters)){
    stop("None of the provided parameters were found in the output")
  }

  n <- length(parameters)
  
  qs <- function(x,y){as.numeric(stats::quantile(x,y))}
  
  means <- tops <- bottoms <-ymin <- ymax <- vector(length=n)
  for (i in 1:n){
    hold <- unlist(x$samples[,parameters[i]])
    means[i] <- mean(hold)
    tops[i] <- qs(hold,quantiles[2])
    bottoms[i] <- qs(hold,quantiles[1])   
  }
  
  ymin <- min(bottoms)
  ymax <- max(tops)
  
  graphics::plot(1:n, means, xaxt="n", 
                 ylim=c(ymin,ymax), xlim=c(0,n+1),
                 xlab="Parameters", ylab="Parameter Values",
                 pch=19, cex=1.5,
                 main=paste('Whisker plot, quantiles (',quantiles[1],
                            ' - ',quantiles[2],')',sep=""))
  graphics::axis(side=1, at=c(1:n), labels=parameters)
  graphics::box()
  
  if(zeroline){graphics::abline(h=0)}
  
  for (i in 1:n){
    graphics::segments(x0=i,y0=bottoms[i], x1=i, y1=tops[i], lwd=2)
    graphics::segments(x0=i-0.2,y0=bottoms[i],x1=i+0.2, y1=bottoms[i])
    graphics::segments(x0=i-0.2,y0=tops[i], x1=i+0.2, y1=tops[i])
  }
 
}
