data_dir <- file.path("..", "Data")
fig_dir <- file.path("..", "Figures")
source("functions/netcascade_JO.R")
source("functions/extinction_cascader.R")

beta.par <- matrix(c(0.1, 1, 0.5, 1, 1, 1, 3, 3, 4, 0.1, 0.2, 0.2), byrow = TRUE, ncol = 2)
rownames(beta.par)=c("exp","power","unif","normal","left", "bimod")
R_options <- c("VA_low", "VA_med", "VA_high", rownames(beta.par))

# path to directory containing empirical networks:
emp_net_path <- file.path(data_dir, "nets_emp")

# load all networks in that directory
emp_nets<-dir(path = emp_net_path) #Plant-pollinator weighted nets

mat_empirical <- list()

for(p in 1:length(emp_nets)){
	mat_empirical[[p]] <- as.matrix(read.table(file.path(emp_net_path, emp_nets[p])))
}
mat_empirical[[4]] <- NULL

R_by_guild <- expand.grid(R_options, R_options)

R_combo_out <- list()

for(Rcombo in 1:nrow(R_by_guild)){
	current_Rrows <- as.character(R_by_guild[Rcombo,1])
	current_Rcols <- as.character(R_by_guild[Rcombo,2])
	

	# DO THE SIMULATIONS ON EACH NETWORK
	stored_events <- list()
	just_extinctions <- list()
	extinctions_by_guild <- list()
	total_extinctions <- list()
	total_cascades <- list()
	cascade_frequency <- list()
	
	for(i in 1:length(mat_empirical)){
	
		mat <- mat_empirical[[i]]
		
		if(min(c(rowSums(mat), colSums(mat))) == 0){
		stop("Hey, you have a species in your network that does not interact with any other species. The following functions will get stuck.")}
		
		m<-nrow(mat)
		n<-ncol(mat)
		
		stored_events[[i]] <- extinction_cascader(imatrix = mat, R_row = current_Rrows, R_col = current_Rcols, nsims = 10000)
		
		just_extinctions[[i]] <- lapply(stored_events[[i]], "[", c("lost_cols", "lost_rows"))
		
		extinctions_by_guild[[i]] <- do.call(rbind, lapply(just_extinctions[[i]], lengths))
		
		total_extinctions[[i]] <- rowSums(extinctions_by_guild[[i]])
		
		total_cascades[[i]] <- sum(total_extinctions[[i]] > 1)
		
		cascade_frequency[[i]] <- total_cascades[[i]]/length(stored_events[[i]])
		
	}

	R_combo_out[[Rcombo]] <- list(stored_events, just_extinctions, extinctions_by_guild, total_extinctions, total_cascades, cascade_frequency)

}

# for 6 networks and 72 R combos: took approximately 3 hrs on 2.3GHz*4 16GBRAM


#--------------------PLOTTING---------------------------------------

# boxplot(sum(number_cascades > 1)/length(number_cascades))


