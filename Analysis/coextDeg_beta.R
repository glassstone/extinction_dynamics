# Modified from original function 'coextDeg' from Vieira and Almeida-Neto 2014 ECOLET

# performs simulations (n = nsims) of the subsequent extinctions following from a primary extinction event in an interaction matrix (imatrix)
# returns how often the extinction sequence ended in each extinction level (i.e. first level, second level, third level, etc.) 
# primary extinctions in each trophic level (e.g. T1 and T2) are drawn from a beta distribution with two shape parameters for each trophic level (beta_par1_T1, beta_par2_T1, beta_par1_T2, beta_par2_T2)


coextDeg_beta <- function(imatrix, nsims, beta_par1_T1 = 1, beta_par2_T1 = 1,beta_par1_T2 = 1, beta_par2_T2 = 1){
  degs <- c()
  for(sim in 1:nsims){
    ranim <- rbeta(n = nrow(imatrix), shape1 = beta_par1_T1, shape2 = beta_par2_T1)
    rplants <- rbeta(n = ncol(imatrix), shape1 = beta_par1_T2, shape2 = beta_par2_T2)
    guild <- sample(c('animal','plant'),1,F,c(nrow(imatrix),ncol(imatrix)))
    if(guild=="animal"){
      target <- rep(1,nrow(imatrix)) #same probability of primary extinction for all spp
    }else{
      target <- rep(1,ncol(imatrix)) #same probability of primary extinction for all spp
    }   
    profiles <- netcascade(imatrix=imatrix,ranim=ranim,rplants=rplants,targetGuild=guild,target=target)
    degs[sim] <- max(profiles[[1]]$degree)
  }
  return(degs)
}






# GRAVEYARD:
   	# sample R values from a distribution set by user
   	# dist_options <- c("normal", "exponential", "uniform")
   	
   	# TODO add a warning later
   	# if(!(dist %in% dist_options))
    		# stop("argument 'dist' must be one of the following: ", dist_options)
    		
    	# if(distr == "normal"){    		
    		# dist_call <- rnorm    		
    	# } else if(distr == "exponential"){    		
    		# dist_call <- rexp
    	# } else if(distr == "uniform"){    		
    		# dist_call <- runif
    	# }
