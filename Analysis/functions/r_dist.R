#!/usr/bin/env Rscript

r_dist <- function(length = 1, type = "unif") {
#===============================================================================
	# this function will return 1 or more values between 0 and 1,
	# corresponding to a set of species' dependence on an interaction
	
	# check if length is a whole number
	if( length %% 1 != 0){
		stop('"length" must be a whole number')
	}
	
	# check that the data frame of options exists
	if(!is.data.frame(beta_par_R)) {
		stop('The data frame "beta_par_R" is missing')
	}
	
	if(!(type %in% beta_par_R$name)) {
		stop(c("'type' must be one of: ", paste(beta_par_R$name, collapse = ", ")))
	}
	
	if(type == "Vogler2001"){
		
		r_values <- rbeta_bimod(n = length, 
			beta1shape1 = beta_par_R[beta_par_R$name == "Vogler2001_L", "par1"], 
			beta1shape2 = beta_par_R[beta_par_R$name == "Vogler2001_L", "par2"], 
			beta2shape1 = beta_par_R[beta_par_R$name == "Vogler2001_R", "par1"], 
			beta2shape2 = beta_par_R[beta_par_R$name == "Vogler2001_R", "par2"]
		)
		
	} else {
		
		r_values <- rbeta(n = length, 
			shape1 = beta_par_R[beta_par_R$name == type, "par1"], 
			shape2 = beta_par_R[beta_par_R$name == type, "par2"]
		)
		
	}
	
	return(r_values)

}

# test_out <- list()
# for(i in beta_par_R$name) {
	# test_out[[i]] <- r_dist(length = 100000, type = i)
# }

# par(mfrow = c(length(beta_par_R$name), 1), mar = c(1,1,2,1))
# for(i in beta_par_R$name) {
	# # plot(density(test_out[[i]]), main = i, xlim = c(0,1))
	# hist(test_out[[i]], main = i, xlim = c(0,1))
# }
