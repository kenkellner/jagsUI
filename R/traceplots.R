
traceplots <- function(x){
  if(class(x)!="simplejags"){stop('Requires simplejags object as input')}
  samples <- x$samples
  params <- names(as.data.frame(samples[[1]]))
  nparams <- length(params)
  nchains <- x$mcmc.info[[1]]
  rhat <- x$Rhat
  
  devAskNewPage(ask=TRUE)
  
  xmax <- x$mcmc.info[[6]] / nchains

  col=c('red','blue','green','yellow','orange','violet')
  if(nchains>6){col=rainbow(nchains)}
  
  for (i in 1:nparams){
    ymax <- range(samples[,i])
    rhat <- gelman.diag(samples[,i])$psrf[1]
    title <- paste('Trace of ',params[i],', Rhat = ',round(rhat,2),sep="")
    plot(x = 1:xmax, y = samples[,i][[1]], main = title, xlab="Iterations", ylab="Value",type="l", col=col[1])
    if(nchains>1){
    for (j in 2:nchains){
      lines(x = 1:xmax, y = samples[,i][[j]],type="l", col=col[j])
    }}
  }
  
  devAskNewPage(ask=FALSE)
  
}