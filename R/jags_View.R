
# Removed View.jagsUI (was already deprecated)

jags.View <- function(x, title, digits=3){
  # grab object name
  if(missing(title)){
    title <- paste("jagsUI:", deparse(substitute(x)))
  }
  # Organize columns
  if(x$mcmc.info$n.chains!=1){y = x$summary[,c(1,2,3,5,7,10,11,8,9)]
  } else {y = x$summary[,c(1,2,3,5,7,10,11)]}
  z <-  as.data.frame(round(as.matrix(y),digits))
  if(is.vector(y)){
    z <- as.data.frame(t(z))
    #row.names(z) <- rownames(x$summary)
  }
  z[,6] <- z[,6]==1

  view.out <- cbind(parameter=rownames(x$summary),z)
  row.names(view.out) <- NULL

  # View the output
  utils::View(view.out, title=title)

  invisible(view.out)
}
