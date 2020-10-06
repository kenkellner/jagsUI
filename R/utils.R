

#--- from process_output ---------------------------------------------------------------------------
#Check that an object is the right class
check_class <- function(output){
  if(!inherits(output, "jagsUI")) stop("Requires jagsUI object")
}
#------------------------------------------------------------------------------
