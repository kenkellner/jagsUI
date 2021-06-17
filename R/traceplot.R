#Get traceplots for series of parameters
traceplot <- function(x, parameters=NULL, Rhat_min=NULL,
                      layout=NULL, ask=NULL){

  #Check input class and get basic plot settings
  check_class(x)
  if(is.null(ask))
    ask <- grDevices::dev.interactive(orNone = TRUE)
  plot_info <- get_plot_info(x, parameters, layout, ask, Rhat_min)

  #Handle par()
  old_par <- graphics::par(plot_info$new_par)
  on.exit(graphics::par(old_par))

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
  graphics::matplot(1:nrow(vals), vals, type='l', lty=1, col=cols,
                 xlab='Iterations', ylab='Value',
                 main=bquote(.(parameter)*","~hat(R) == .(Rhat)))

  #Add margin labels if necessary
  if(m_labels){
    graphics::mtext("Iteration", side=1, line=1.5, outer=TRUE)
    graphics::mtext("Value", side=2, line=1.5, outer=TRUE)
  }
}

#General function for setting up plots
# get_plot_info now has its own file
