#Get traceplots for series of parameters
traceplot <- function(x, parameters=NULL, Rhat_min=NULL,
                      per_plot=9, ask=grDevices::dev.interactive(orNone = TRUE)){

  #Check input class and get basic plot settings
  check_class(x)
  plot_info <- get_plot_info(x, parameters, per_plot, ask, Rhat_min)

  #Handle par()
  old_par <- graphics::par(no.readonly=TRUE)
  on.exit(graphics::par(old_par))
  graphics::par(plot_info$new_par)

  #Generate plot
  n <- length(plot_info$params)
  for (i in 1:n){
    m_labels <- (i %% plot_info$per_plot == 0) || (i==n)
    param_trace(x, plot_info$params[i], m_labels=m_labels)
  }

}

#Traceplot for single parameter
param_trace <- function(x, parameter, m_labels=FALSE){

  #Get samples and Rhat values
  vals <- mcmc_to_mat(x$samples, parameter)
  Rhat <- sprintf("%.3f",round(x$summary[parameter, 'Rhat'],3))

  #Draw plot
  cols <- grDevices::rainbow(ncol(vals))
  graphics::plot(1:nrow(vals), vals[,1], type='l', col=cols[1],
                 ylim=range(vals), xlab='Iterations', ylab='Value',
                 main=paste('Trace of',parameter))
  for (i in 2:ncol(vals)) graphics::lines(1:nrow(vals), vals[,i], col=cols[i])

  #Add Rhat value
  graphics::legend('bottomright', legend=bquote(hat(R) == .(Rhat)),
                   bty='o', bg='white', cex=1.2)

  #Add margin labels if necessary
  if(m_labels){
    graphics::mtext("Iteration", side=1, line=1.5, outer=TRUE)
    graphics::mtext("Value", side=2, line=1.5, outer=TRUE)
  }
}

#General function for setting up plots
get_plot_info <- function(x, parameters, per_plot, ask, Rhat_min=NULL){

  #Expand non-scalar parameters and check they exist
  all_params <- param_names(x$samples)
  if(!is.null(parameters)){
    parameters <- match_params(parameters, all_params)
    if(is.null(parameters)){
      stop("None of the provided parameters were found in the output")
    }
  } else{
    parameters <- all_params
  }

  #If rhat_min, check parameters against it
  if(!is.null(Rhat_min)){
    Rhats <- x$summary[parameters, 'Rhat']
    parameters <- parameters[Rhats >= Rhat_min]
    if(length(parameters)==0) stop("No parameters > Rhat_min")
  }

  #Reduce max panels per plot if larger than number of parameters
  if(length(parameters) <= per_plot){
    per_plot <- length(parameters)
    ask=FALSE
  }

  #Set up new par settings
  dims <- c(ceiling(sqrt(per_plot)), round(sqrt(per_plot)))
  new_par <- list(mfrow=dims, mar=c(1.5,1.5,2.5,1), oma=c(3,3,0,0),
                ask=ask)

  list(params=parameters, new_par=new_par, per_plot=per_plot)
}
