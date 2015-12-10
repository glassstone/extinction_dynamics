

extinction_cascader <- function(imatrix, R_row, R_col, nsims){
	beta.par <- matrix(c(0.1, 1, 0.5, 1, 1, 1, 3, 3, 4, 0.1, 0.2, 0.2), byrow = TRUE, ncol = 2)
	rownames(beta.par)=c("exp","power","unif","normal","left", "bimod")
	R_options <- c("VA_low", "VA_med", "VA_high", rownames(beta.par))
	if(R_row %in% R_options == FALSE || R_col %in% R_options == FALSE ){stop(paste('R_row and R_col must be one of these: ', paste(R_options, paste = " ")))}
	
	cascaded <- list()
	for(i in 1:nsims){
		switch(R_row, 
			VA_low = rep(runif(1, min = 0, max = 0.3), nrow(imatrix)), 
			VA_med = rep(runif(1, min = 0.3, max = 0.6), nrow(imatrix)), 
			VA_high = rep(runif(1, min = 0.6, max = 1), nrow(imatrix)), 
			exp = rbeta(nrow(imatrix), shape1 = beta.par["exp", 1], shape2 = beta.par["exp", 2]), 
			power = rbeta(nrow(imatrix), shape1 = beta.par["power", 1], shape2 = beta.par["power", 2]), 
			unif = rbeta(nrow(imatrix), shape1 = beta.par["unif", 1], shape2 = beta.par["unif", 2]), 
			normal = rbeta(nrow(imatrix), shape1 = beta.par["normal", 1], shape2 = beta.par["normal", 2]), 
			left = rbeta(nrow(imatrix), shape1 = beta.par["left", 1], shape2 = beta.par["left", 2]), 
			bimod = rbeta(nrow(imatrix), shape1 = beta.par["bimod", 1], shape2 = beta.par["bimod", 2])
		)

		switch(R_col, 
			VA_low = rep(runif(1, min = 0, max = 0.3), ncol(imatrix)), 
			VA_med = rep(runif(1, min = 0.3, max = 0.6), ncol(imatrix)), 
			VA_high = rep(runif(1, min = 0.6, max = 1), ncol(imatrix)), 
			exp = rbeta(ncol(imatrix), shape1 = beta.par["exp", 1], shape2 = beta.par["exp", 2]), 
			power = rbeta(ncol(imatrix), shape1 = beta.par["power", 1], shape2 = beta.par["power", 2]), 
			unif = rbeta(ncol(imatrix), shape1 = beta.par["unif", 1], shape2 = beta.par["unif", 2]), 
			normal = rbeta(ncol(imatrix), shape1 = beta.par["normal", 1], shape2 = beta.par["normal", 2]), 
			left = rbeta(ncol(imatrix), shape1 = beta.par["left", 1], shape2 = beta.par["left", 2]), 
			bimod = rbeta(ncol(imatrix), shape1 = beta.par["bimod", 1], shape2 = beta.par["bimod", 2])
		)
		
		unluckyGuildArg <- sample(x = c("rows", "cols"), size = 1, prob = c(nrow(imatrix), ncol(imatrix)))
		if(unluckyGuildArg == "rows"){
	      unluckySpeciesArg <- rep(1,nrow(imatrix))
	    }else{
	      unluckySpeciesArg <- rep(1,ncol(imatrix))
	    }   



		cascaded[[i]] <- netcascadeJO(
									imatrix = imatrix, 
									R_rows = R_row, 
									R_cols = R_col, 
									unluckyGuild = unluckyGuildArg,
									unluckySpecies = unluckySpeciesArg, 
									return.matrix = TRUE
								)

	}
	
	return(cascaded)
}
