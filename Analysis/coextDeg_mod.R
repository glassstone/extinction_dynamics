#coextMaxLvl performs simulations (n = nsims) of a single episode of primary extinction and its possible associated coextinction cascade and returns how often the extinction sequence ended in each extinction level (i.e. first level, second level, third level, etc.) 
#primary extinctions are uniform random among both groups.'r' is sampled from a interval (rlow, rup)

# we should add dist_T1 and dist_T2 for each of the trophic levels T1 and T2.

coextDeg <- function(imatrix, nsims, beta_par1 = 1, beta_par2 = 1){
  degs <- c()
  for(sim in 1:nsims){
    ranim <- rbeta(n = nrow(imatrix), shape1 = beta_par1, shape2 = beta_par2)
    rplants <- rbeta(n = ncol(imatrix), shape1 = beta_par1, shape2 = beta_par2)
    guild <- sample(c('animal','plant'),1,F,c(nrow(imatrix),ncol(imatrix)))
    if(guild=="animal"){
      target <- rep(1,nrow(imatrix))
    }else{
      target <- rep(1,ncol(imatrix))
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
