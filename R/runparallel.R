
run.parallel <- function(data=NULL,inits=NULL,parameters.to.save,model.file=NULL,n.chains,n.adapt,n.iter,n.burnin,n.thin,
                         modules,seed,DIC,model.object=NULL,update=FALSE,verbose=TRUE) {

#Set number of clusters/chains
p <- detectCores()
if(n.chains > p){
  stop('Number of chains (',n.chains,') exceeds available cores (',p,'), reduce number of chains.',sep="")
} else {n.cluster <- n.chains}

#Set random seed
set.seed(seed)

#Set up clusters
cl = makeCluster(n.cluster)
clusterExport(cl = cl, ls(), envir = environment())
clusterSetRNGStream(cl, seed)

if(verbose){
cat('Beginning parallel processing with',n.cluster,'clusters. Console output will be suppressed.\n')}

#Function called in each cluster
jags.clust <- function(i){

#Load modules
set.modules(modules,DIC)

if(update){
  #Recompile model
  cluster.mod <- model.object[[i]]
  
  #Run model
  rjags.output <- run.model(model.file=NULL,data=NULL,inits=NULL,parameters.to.save,n.chains=1,n.iter,n.burnin=0,n.thin,n.adapt,
                            verbose=FALSE,model.object=cluster.mod,update=TRUE,parallel=TRUE)
   
} else {

  #Set initial values for cluster
  cluster.inits <- inits[[i]]

  #Run model

  rjags.output <- run.model(model.file,data,inits=cluster.inits,parameters.to.save,n.chains=1,n.iter,
                  n.burnin,n.thin,n.adapt,verbose=FALSE,parallel=TRUE)
  
}

return(list(samp=rjags.output$samples[[1]],mod=rjags.output$m))

}

#Do analysis
on.exit(closeAllConnections())
par <- clusterApply(cl=cl,x=1:n.chains,fun=jags.clust)

#Create empty lists
out <- samples <- model <- list()

#Save samples and model objects from each cluster
for (i in 1:n.cluster){
  samples[[i]] <- par[[i]][[1]]
  model[[i]] <- par[[i]][[2]]
}
out$samples <- as.mcmc.list(samples)
out$model <- model
names(out$model) <- sapply(1:length(out$model),function(i){paste('cluster',i,sep="")})

if(verbose){
cat('\nParallel processing completed.\n\n')
}

return(out)

}



