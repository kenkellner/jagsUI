#Check that an object is the right class---------------------------------------
check_class <- function(output){
  if(!inherits(output, "jagsUI")) stop("Requires jagsUI object")
}

#General function for setting up plots-----------------------------------------
# Called by densityplot, traceplot, and plot.jagsUI
# plot.jagsUI only uses the 'params' component in the output, ignores the rest
get_plot_info <- function(x, parameters, layout, ask, Rhat_min=NULL){

  #Expand non-scalar parameters and check they exist
  all_params <- rownames(x$summary)
  if(!is.null(parameters)){
    #Expand bracketed parameter names
    parameters <- expand_params(parameters)
    #Check parameters are in output
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

  # Fix 'layout'
  if(is.null(layout)) {
    if(length(parameters) <= 9){
      per_plot0 <- length(parameters)
      ask <- FALSE
      layout <- c(ceiling(sqrt(per_plot0)), round(sqrt(per_plot0)))
    } else {
      layout <- c(3,3)
    }
  } else if(length(layout) != 2) {
    layout <- rep(layout[1], 2)
  }
  per_plot <- prod(layout)

  #Set up new par settings
  new_par <- list(ask=ask)
  if(per_plot > 1) {
    new_par$mfrow <- layout
    new_par$oma <- c(3,3,0,0)
    new_par$mar <- c(1.5,1.5,2.5,1)
  }

  list(params=parameters, new_par=new_par, per_plot=per_plot)
}

# Parameter name tools---------------------------------------------------------
expand_params <- function(params){
  unlist(lapply(params, expand_brackets))
}

expand_brackets <- function(x){
  if(!has_brackets(x)) return(x)

  pname <- strsplit(x, "\\[")[[1]][1]
  rng <- gsub(paste0(pname,"|\\[|\\]"), "", x)
  rng <- eval(parse(text=rng))
  paste0(pname, "[",rng,"]")
}

has_brackets <- function(x){
  grepl("\\[.*\\]", x)
}
