#Get density plots for series of parameters
densityplot <- function(x, parameters=NULL, per_plot=9, ask=grDevices::dev.interactive(orNone = TRUE)){
  
  #Check input class and get basic plot settings
  check_class(x)  
  plot_info <- get_plot_info(x, parameters, per_plot, ask)
  
  #Handle par()
  old_par <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(old_par))  
  graphics::par(plot_info$new_par)
  
  #Generate plot
  n <- length(plot_info$params)
  for (i in 1:n){
    m_labels <- (i %% plot_info$per_plot == 0) || (i==n)
    param_density(x, plot_info$params[i], m_labels=m_labels)
  }
}

#Density plot for single parameter
param_density <- function(x, parameter, m_labels=FALSE){
  
  #Get samples
  vals <- mcmc_to_mat(x$samples, parameter)
  
  #Get densities
  dens <- lapply(1:ncol(vals), function(x) stats::density(vals[,x]))

  #Get plot limits
  xlims <- range(unlist(lapply(dens, function(d) range(d$x))))
  ylims <- range(unlist(lapply(dens, function(d) range(d$y))))

  #Draw plot
  cols <- grDevices::rainbow(ncol(vals))
  graphics::plot(dens[[1]], type='l', col=cols[1], 
                 xlim=xlims, ylim=ylims, xlab='Value', ylab='Density',
                 main=paste('Density of',parameter))
  for (i in 2:ncol(vals)) graphics::lines(dens[[i]], col=cols[i])

  #Add margin labels if necessary
  if(m_labels){
    graphics::mtext("Value", side=1, line=1.5, outer=TRUE)
    graphics::mtext("Density", side=2, line=1.5, outer=TRUE)
  }
  
}
