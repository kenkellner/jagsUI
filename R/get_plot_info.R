
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
  new_par <- list(mar=c(1.5,1.5,2.5,1), oma=c(3,3,0,0), ask=ask)
  if(per_plot > 1)
    new_par$mfrow <- c(ceiling(sqrt(per_plot)), round(sqrt(per_plot)))

  list(params=parameters, new_par=new_par, per_plot=per_plot)
}
