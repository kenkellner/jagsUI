#Get density plots for series of parameters
densityplot <- function(x, parameters=NULL, layout=NULL, ask=NULL){
  
  #Check input class and get basic plot settings
  check_class(x)  
  if(is.null(ask))
    ask <- grDevices::dev.interactive(orNone = TRUE)
  plot_info <- get_plot_info(x, parameters, layout, ask)
  
  #Handle par()
  old_par <- graphics::par(plot_info$new_par)
  on.exit(graphics::par(old_par))  
  
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
  vals <- mcmc_to_mat(x$samples[,parameter])
 
  if(any(is.na(vals))){
    graphics::plot(1:nrow(vals), rep(0, nrow(vals)), type='n',
         xlab='Value', ylab='Density', main=paste('Density of',parameter))
  } else {
    # Get bandwidth, one value for all chains
    bw <- mean(apply(vals, 2, stats::bw.nrd0))

    from <- min(vals) - 3*bw  # these are 'density's defaults...
    to <- max(vals) + 3*bw    # ... use these if no constraints
    xx <- vals
    mult <- 1

    # Check for non-negative constraint or probability
    if (min(vals) >= 0 && min(vals) < 2 * bw) { # it's non-negative
      from <- 0
      xx <- rbind(vals, -vals)
      mult <- 2
    }
    if (min(vals) >= 0 && max(vals) <= 1 &&
        (min(vals) < 2 * bw || 1 - max(vals) < 2 * bw)) { # it's a probability
      xx <- rbind(vals, -vals, 2-vals)
      mult <- 3
      to <- 1
    }

    # Get densities
    dens <- apply(xx, 2, function(x) stats::density(x, bw=bw, from=from, to=to)$y) * mult

    # Draw plot
    x <- seq(from, to, length=nrow(dens))
    cols <- grDevices::rainbow(ncol(vals))
    graphics::matplot(x, dens, type='l', lty=1, col=cols,
        xlab='Value', ylab='Density', main=paste('Density of',parameter))
  }

  #Add margin labels if necessary
  if(m_labels){
    graphics::mtext("Value", side=1, line=1.5, outer=TRUE)
    graphics::mtext("Density", side=2, line=1.5, outer=TRUE)
  }
  
}
