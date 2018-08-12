
summary.matrix <- function(output,samples,n.chains,codaOnly){
  
  hold <- unlist(output$mean[!names(output$mean)%in%codaOnly])
  toremove <- which(!is.na(hold))

  #Get sorted names
  sort.names = c()
  for (i in 1:length(output$mean)){
    if(length(output$mean[[i]])>1){
      raw.ind <- which(output$mean[[i]]==output$mean[[i]],arr.ind=T)
      if(is.matrix(raw.ind)){
        ind <- apply(raw.ind,1,paste,collapse=',')
      } else {
        ind <- raw.ind
      }

      newnames <- paste(names(output$mean)[i],'[',ind,']',sep='')
      sort.names <- c(sort.names,newnames)

    } else {
      sort.names <- c(sort.names,names(output$mean[i]))
    }
  }


  rnames <- colnames(samples[[1]])
  sorted.order <- order(match(rnames,sort.names))
  rnames <- rnames[sorted.order]
  
  cleanup <- function(input,codaOnly){

    out.raw <- unlist(input[!names(input)%in%codaOnly])
    
    out <- out.raw[toremove]
    return(out)
  }
  
  y = data.frame(cleanup(output$mean,codaOnly),cleanup(output$sd,codaOnly),
                 cleanup(output$q2.5,codaOnly),cleanup(output$q25,codaOnly),
                 cleanup(output$q50,codaOnly),cleanup(output$q75,codaOnly),
                 cleanup(output$q97.5,codaOnly),
                 cleanup(output$Rhat,codaOnly),cleanup(output$n.eff,codaOnly),
                 cleanup(output$overlap0,codaOnly),cleanup(output$f,codaOnly))
 
  p <- rnames
  expand <- sapply(strsplit(p, "\\["), "[", 1)  
  row.names(y) = p[!expand%in%codaOnly]
  names(y) = c('mean','sd','2.5%','25%','50%','75%','97.5%','Rhat','n.eff','overlap0','f')
  if(n.chains==1){
    y = y[,-c(8,9)]
  }
  
  return(as.matrix(y))
}
