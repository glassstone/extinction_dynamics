N_iterations <- 10


test_mat <- mat_empirical[[3]]

do.call(rbind, lapply(replicate(10, netcascade_JO(test_mat, 1,1,"rows", 2), simplify = FALSE), "[[", 2))

stored_events <- list()

for(i in 1:length(R_options)) {

	for(j in 1:length(R_options)) {
		
			R_ROW <- R_options[i]
			
			R_COL <- R_options[j]
			
			print(c(R_ROW, R_COL))

			stored_events[[i]][[j]] <- replicate(N_iterations,
				netcascade_JO(test_mat, 
					R_rows = r_dist(length = nrow(test_mat), type = R_ROW), 
					R_cols = r_dist(length = ncol(test_mat), type = R_COL), 
					unluckyGuild = "random_binary",
					unluckySpecies = 1,
					extinct_cols = NULL, 
					extinct_rows = NULL, 
					return.matrix = FALSE
				), 
				simplify = FALSE
			)
			

	}

}
