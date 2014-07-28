
summary.matrix <- function(output,samples,n.chains,codaOnly){
  
  y = data.frame(unlist(output$mean[!names(output$mean)%in%codaOnly]),unlist(output$sd[!names(output$mean)%in%codaOnly]),
                 unlist(output$q2.5[!names(output$mean)%in%codaOnly]),unlist(output$q25[!names(output$mean)%in%codaOnly]),
                 unlist(output$q50[!names(output$mean)%in%codaOnly]),unlist(output$q75[!names(output$mean)%in%codaOnly]),
                 unlist(output$q97.5[!names(output$mean)%in%codaOnly]),
                 unlist(output$Rhat[!names(output$mean)%in%codaOnly]),unlist(output$n.eff[!names(output$mean)%in%codaOnly]),
                 unlist(output$overlap0[!names(output$mean)%in%codaOnly]),unlist(output$f[!names(output$mean)%in%codaOnly])) 
  p <- colnames(samples[[1]])
  expand <- sapply(strsplit(p, "\\["), "[", 1)  
  row.names(y) = p[!expand%in%codaOnly]
  names(y) = c('mean','sd','2.5%','25%','50%','75%','97.5%','Rhat','n.eff','overlap0','f')
  if(n.chains==1){
    y = y[,-c(8,9)]
  }
  
  return(as.matrix(y))
}