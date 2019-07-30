## INIT PROCESSING-------------------------------------------------------------

get_inits <- function(raw_inits, n_chains){
  
  all_list <- function(l) all(sapply(l,class)=='list')

  if(is.null(raw_inits)){
    #Make blank list of lists to add RNG to later
    inits <- replicate(n_chains,list())
  } else if(is.function(raw_inits)){
    #If function provided, run it to generate values
    inits <- replicate(n_chains,raw_inits(),simplify=F)
    #Check to make sure the result is a list of lists
    if(!all_list(inits)){
      stop('Inits function must return a list')
    }
  } else if(is.list(raw_inits)){
    #If list provided, check it is the right length
    if(length(raw_inits)!=n_chains){
      stop('length(inits) != number of chains')
    } else if(!all_list(raw_inits)){
      #Check all elements are lists
      stop('All elements of inits must be lists')
    } else {
      inits <- raw_inits
    }
  } else {
    stop('If provided, inits must be a function or a list of lists')
  }

  inits <- add_RNG(inits)

  inits
}
#------------------------------------------------------------------------------

## Add RNG seed to inits list--------------------------------------------------
add_RNG <- function(inits_list){
  #Don't replace existing RNG seed if it exists
  if(".RNG.seed" %in% names(inits_list[[1]])) return(inits_list)
  
  for (i in 1:length(inits_list)){
    inits_list[[i]]$.RNG.name="base::Mersenne-Twister"
    inits_list[[i]]$.RNG.seed=sample(1:1e5,1)
  }
  inits_list
}
