
whiskerplot <- function(x,parameters,quantiles=c(0.025,0.975),
                        zeroline=TRUE, ...){
  
  #Check input object class
  check_class(x)
  
  #Check parameters given
  all_params <- param_names(x$samples)
  parameters <- match_params(parameters, all_params)
  if(is.null(parameters)){
    stop("None of the provided parameters were found in the output")
  }
  
  #Check quantile argument
  if((length(quantiles)!=2) | (quantiles[2] <= quantiles[1])){
    stop("Incompatible quantile values provided")
  }

  #Calculate means and CIs
  post_stats <- sapply(parameters, 
                       function(i){
                          sims <- mcmc_to_mat(x$samples, i)
                          c(mean(sims,na.rm=TRUE), 
                            stats::quantile(sims,na.rm=TRUE,quantiles))
                        })

  #Plot parameter means
  n <- length(parameters)
  graphics::plot(1:n, post_stats[1,], xaxt="n", 
                 ylim=range(post_stats), xlim=c(0,n+1),
                 xlab="Parameters", 
                 ylab=paste0('Parameter mean and quantiles (',quantiles[1],
                            ' - ',quantiles[2],')'), pch=19, cex=1.5, ...)
  graphics::axis(side=1, at=1:n, labels=parameters)
  graphics::box()
  
  #Draw line at zero
  if(zeroline) graphics::abline(h=0)
  
  #Draw error bars
  wd <- (n+2)/40
  graphics::segments(1:n, post_stats[2,], 1:n, post_stats[3,], lwd=2)
  graphics::segments(1:n-wd, post_stats[2,], 1:n+wd, post_stats[2,])
  graphics::segments(1:n-wd, post_stats[3,], 1:n+wd, post_stats[3,])
}
