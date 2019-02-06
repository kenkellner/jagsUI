
run.parallel <- function(data=NULL,inits=NULL,parameters.to.save,model.file=NULL,n.chains,n.adapt,n.iter,n.burnin,n.thin,
                         modules,factories,DIC,model.object=NULL,update=FALSE,verbose=TRUE,n.cores=NULL) {

#Save current library paths
current.libpaths <- .libPaths()

#Set up clusters
cl = makeCluster(n.cores)
on.exit(stopCluster(cl))
clusterExport(cl = cl, ls(), envir = environment())
clusterEvalQ(cl,.libPaths(current.libpaths))

if(verbose){
cat('Beginning parallel processing using',n.cores,'cores. Console output will be suppressed.\n')}

#Function called in each core
jags.clust <- function(i){

#Load modules
set.modules(modules,DIC)
set.factories(factories)

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

return(list(samp=rjags.output$samples[[1]],mod=rjags.output$m,total.adapt=rjags.output$total.adapt,sufficient.adapt=rjags.output$sufficient.adapt))

}

#Do analysis
par <- clusterApply(cl=cl,x=1:n.chains,fun=jags.clust)

#Create empty lists
out <- samples <- model <- list()
total.adapt <- sufficient.adapt <- vector(length=n.chains)

#Save samples and model objects from each cluster
for (i in 1:n.chains){
  samples[[i]] <- coda::mcmc(par[[i]][[1]],start=n.burnin+n.thin,thin=n.thin)
  model[[i]] <- par[[i]][[2]]
  total.adapt[i] <- par[[i]][[3]]
  sufficient.adapt[i] <- par[[i]][[4]]
}
out$samples <- as.mcmc.list(samples)
out$model <- model
out$total.adapt <- total.adapt
out$sufficient.adapt <- sufficient.adapt
names(out$model) <- sapply(1:length(out$model),function(i){paste('cluster',i,sep="")})

if(verbose){
cat('\nParallel processing completed.\n\n')
}

return(out)

}



