
run.parallel <- function(data,inits,parameters.to.save,model.file,n.chains,n.adapt,n.iter,n.burnin,n.thin,
                         seed,DIC) {

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

cat('Beginning parallel processing with',n.cluster,'clusters. Console output will be suppressed.\n')


#Function called in each cluster
jags.clust <- function(i){

#Set initial values for cluster
cluster.inits <- inits[[i]]

#Load rjags and modules
require(rjags)
if(DIC){
  load.module("dic",quiet=TRUE)
}

#Compile model
m <- jags.model(file=model.file,data=data,inits=cluster.inits,n.chains=1,n.adapt=0)

#Adapt using adapt()
if(n.adapt>0){
x <- adapt(object=m,n.iter=n.adapt,progress.bar="none",end.adaptation=TRUE)
} else{
x <- adapt(object=m,n.iter=1,end.adaptation=TRUE)
}

#Burn-in phase using update()  
if(n.burnin>0){
  update(object=m,n.iter=n.burnin,progress.bar="text")} 

#Sample from posterior using coda.samples() 
samples <- coda.samples(model=m,variable.names=parameters.to.save,n.iter=(n.iter-n.burnin),thin=n.thin,
                        progress.bar="none")

return(list(samp=samples[[1]],mod=m))

}

#Do analysis
par <- clusterApply(cl=cl,x=1:n.chains,fun=jags.clust)
closeAllConnections()

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

cat('\nParallel processing completed.\n\n')

return(out)

}



