
#Plot method for jagsUI objects
plot.jagsUI <- function(x, parameters=NULL, per_plot=4, ask=NULL, ...){

  if(is.null(ask))
    ask <- grDevices::dev.interactive(orNone = TRUE)
  plot_info <- get_plot_info(x, parameters, NULL, ask)
  dims <- c(min(length(plot_info$params), per_plot), 2)
  if(length(plot_info$params) <= per_plot)
    ask <- FALSE
  new_par <- list(mfrow = dims, mar = c(4,4,2.5,1), oma=c(0,0,0,0), ask=ask)
   
  #Handle par()
  old_par <- graphics::par(new_par)
  on.exit(graphics::par(old_par))  
  

  #Make plot
  for (i in plot_info$params){
    param_trace(x, i)
    param_density(x, i)
  }
}
