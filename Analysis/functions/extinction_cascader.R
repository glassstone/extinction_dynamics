

extinction_cascader <- function(imatrix, R_row, R_col, nsims){
	beta.par <- matrix(c(0.1, 1, 0.5, 1, 1, 1, 3, 3, 4, 0.1, 0.2, 0.2), byrow = TRUE, ncol = 2)
	rownames(beta.par)=c("exp","power","unif","normal","left", "bimod")
	R_options <- c("VA_low", "VA_med", "VA_high", rownames(beta.par))
	
	cascaded <- list()
	for(i in 1:nsims){
		if(R_row == "VA_low"){
			R_val_row <- rep(runif(1, min = 0, max = 0.3), nrow(imatrix))
		} else if(R_row == "VA_med"){
			R_val_row <- rep(runif(1, min = 0.3, max = 0.6), nrow(imatrix))
		} else if(R_row == "VA_high"){
			R_val_row <- rep(runif(1, min = 0.6, max = 1), nrow(imatrix))
		} else if(R_row == "exp"){
			R_val_row <- rbeta(nrow(imatrix), shape1 = beta.par["exp", 1], shape2 = beta.par["exp", 2])
		} else if(R_row == "power"){
			R_val_row <- rbeta(nrow(imatrix), shape1 = beta.par["power", 1], shape2 = beta.par["power", 2])
		} else if(R_row == "unif"){
			R_val_row <- rbeta(nrow(imatrix), shape1 = beta.par["unif", 1], shape2 = beta.par["unif", 2])
		} else if(R_row == "normal"){
			R_val_row <- rbeta(nrow(imatrix), shape1 = beta.par["normal", 1], shape2 = beta.par["normal", 2])
		} else if(R_row == "left"){
			R_val_row <- rbeta(nrow(imatrix), shape1 = beta.par["left", 1], shape2 = beta.par["left", 2])
		} else if(R_row == "bimod"){
			R_val_row <- rbeta(nrow(imatrix), shape1 = beta.par["bimod", 1], shape2 = beta.par["bimod", 2])
		} else {
			stop("could not evaluate the R_row argument to extinction_cascader")
		}
		# print(R_val_row)

		# R_col <- switch(R_col, 
			# VA_low = rep(runif(1, min = 0, max = 0.3), ncol(imatrix)), 
			# VA_med = rep(runif(1, min = 0.3, max = 0.6), ncol(imatrix)), 
			# VA_high = rep(runif(1, min = 0.6, max = 1), ncol(imatrix)), 
			# exp = rbeta(ncol(imatrix), shape1 = beta.par["exp", 1], shape2 = beta.par["exp", 2]), 
			# power = rbeta(ncol(imatrix), shape1 = beta.par["power", 1], shape2 = beta.par["power", 2]), 
			# unif = rbeta(ncol(imatrix), shape1 = beta.par["unif", 1], shape2 = beta.par["unif", 2]), 
			# normal = rbeta(ncol(imatrix), shape1 = beta.par["normal", 1], shape2 = beta.par["normal", 2]), 
			# left = rbeta(ncol(imatrix), shape1 = beta.par["left", 1], shape2 = beta.par["left", 2]), 
			# bimod = rbeta(ncol(imatrix), shape1 = beta.par["bimod", 1], shape2 = beta.par["bimod", 2])
		# )
		
		if(R_col == "VA_low"){
			R_val_col <- rep(runif(1, min = 0, max = 0.3), ncol(imatrix))
		} else if(R_col == "VA_med"){
			R_val_col <- rep(runif(1, min = 0.3, max = 0.6), ncol(imatrix))
		} else if(R_col == "VA_high"){
			R_val_col <- rep(runif(1, min = 0.6, max = 1), ncol(imatrix))
		} else if(R_col == "exp"){
			R_val_col <- rbeta(ncol(imatrix), shape1 = beta.par["exp", 1], shape2 = beta.par["exp", 2])
		} else if(R_col == "power"){
			R_val_col <- rbeta(ncol(imatrix), shape1 = beta.par["power", 1], shape2 = beta.par["power", 2])
		} else if(R_col == "unif"){
			R_val_col <- rbeta(ncol(imatrix), shape1 = beta.par["unif", 1], shape2 = beta.par["unif", 2])
		} else if(R_col == "normal"){
			R_val_col <- rbeta(ncol(imatrix), shape1 = beta.par["normal", 1], shape2 = beta.par["normal", 2])
		} else if(R_col == "left"){
			R_val_col <- rbeta(ncol(imatrix), shape1 = beta.par["left", 1], shape2 = beta.par["left", 2])
		} else if(R_col == "bimod"){
			R_val_col <- rbeta(ncol(imatrix), shape1 = beta.par["bimod", 1], shape2 = beta.par["bimod", 2])
		} else {
			stop("could not evaluate the R_col argument to extinction_cascader")
		}

		unluckyGuildArg <- sample(x = c("rows", "cols"), size = 1, prob = c(nrow(imatrix), ncol(imatrix)))
		if(unluckyGuildArg == "rows"){
	      unluckySpeciesArg <- rep(1,nrow(imatrix))
	    }else{
	      unluckySpeciesArg <- rep(1,ncol(imatrix))
	    }


		cascaded[[i]] <- netcascade_JO(
									imatrix = imatrix, 
									R_rows = R_val_row, 
									R_cols = R_val_col, 
									unluckyGuild = unluckyGuildArg,
									unluckySpecies = unluckySpeciesArg, 
									return.matrix = TRUE
								)

	}
	
	return(cascaded)
}
