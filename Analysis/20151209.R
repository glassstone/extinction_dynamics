
# First, you must set your working directory to extinctions/Analysis

# point to the data directory
data_dir <- file.path("..", "Data")

# point to the figure directory to write figures
fig_dir <- file.path("..", "Figures")

# load the functions - this will source all files ending in '.R'
sapply(list.files(pattern="[.]R$", path="functions/", full.names=TRUE), source)
# source("functions/netcascade_JO.R")
# source("functions/extinction_cascader.R")



# path to directory containing empirical networks:
emp_net_path <- file.path(data_dir, "nets_emp")
# load all networks in that directory
emp_net_files <- dir(path = emp_net_path, pattern = "*.txt") #Plant-pollinator weighted nets
emp_nets <- list()
for(p in 1:length(emp_net_files)){
	emp_nets[[p]] <- as.matrix(read.table(file.path(emp_net_path, emp_net_files[p])))
}


# NCEAS IWDB pollination networks
poll_net_path <- file.path(data_dir, "NCEAS_text_files/plant_pollinator")
# skip "kevan_f.txt"
poll_net_files <- dir(path = poll_net_path, pattern = "*.txt")
poll_net_files <- poll_net_files[!poll_net_files == "kevan_f.txt"] # this file is not square (has a row with not enough columns)
poll_nets <- list()
for(p in 1:length(poll_net_files)){
	poll_nets[[p]] <- as.matrix(read.table(file.path(poll_net_path, poll_net_files[p])))
}

# NCEAS IWDB seed disperser networks
seed_net_path <- file.path(data_dir, "NCEAS_text_files/seed_disperser")
seed_net_files <- dir(path = seed_net_path, pattern = "*.txt") 
seed_nets <- list()
for(p in 1:length(seed_net_files)){
	seed_nets[[p]] <- as.matrix(read.table(file.path(seed_net_path, seed_net_files[p])))
}

# NCEAS IWDB seed disperser networks
antplant_net_path <- file.path(data_dir, "NCEAS_text_files/ant_plant")
antplant_net_files <- dir(path = antplant_net_path, pattern = "*.txt") 
antplant_nets <- list()
for(p in 1:length(antplant_net_files)){
	antplant_nets[[p]] <- as.matrix(read.table(file.path(antplant_net_path, antplant_net_files[p])))
}


###########################################################################
# Set the type of network to run
###########################################################################

# alternate, to use all of them:
mat_empirical <- c(poll_nets, seed_nets, antplant_nets)
# mat_empirical <- poll_nets

length(mat_empirical) # 42 for all

# remove any non-numeric networks (e.g. some of them contain "many", "few", "several")
mat_empirical <- mat_empirical[which(sapply(mat_empirical, is.numeric))]
length(mat_empirical) # removes 1


# remove any networks that aren't weighted (i.e. max > 1) 
# test: mat_empirical <- c(mat_empirical, list(as.matrix(read.table(file.path(data_dir, "NCEAS_text_files/plant_pollinator/arr_1_matr.txt")))))
mat_empirical[
	which(
		sapply(
			mat_empirical, 
			max
		) < 2
	)
] <- NULL
length(mat_empirical) # 11 (+3 seed)



# remove rows or columns that doesn't interact with anyone 
# this function will remove species that do not have any interactions
prune_absent <- function(this_mat){
	this_mat <- this_mat[which(rowSums(this_mat) != 0) , ]
	this_mat <- this_mat[ , which(colSums(this_mat) != 0)]
	return(this_mat)
}

mat_empirical <- lapply(mat_empirical, prune_absent)
length(mat_empirical) # no full networks are lost, only species

# mat_empirical[which(sapply(mat_empirical, function(x) min(c(rowSums(x), colSums(x)))) == 0 )] <- NULL





###########################################################
#------------- SET R OPTIONS
###########################################################
# We conducted simulations by assignment
beta.par <- matrix(
	c(
		0.1, 1, 
		0.5, 1, 
		1, 1, 
		3, 3, 
		4, 0.1, 
		0.2, 0.2
		), byrow = TRUE, ncol = 2)
		
rownames(beta.par)=c("exp","power","unif","normal","left", "bimod")
R_options <- c("VA_low", "VA_med", "VA_high", rownames(beta.par))

R_options_full <- expand.grid(row_R = R_options, col_R = R_options, stringsAsFactors = FALSE)

R_scenario <- paste(R_options_full[,"row_R"], R_options_full[,"col_R"], sep = "-")

R_options_full <- data.frame(scenario, R_options_full)

# this is now 'scenario'
# R_combo_names <- apply(X = as.matrix(R_options_full), MARGIN = 1, FUN = function(x) paste(x, sep = "", collapse = "-"))


flip_reverse <- function(x){
	paste(rev(strsplit(x, split = "-")[[1]]), collapse = "-")
}
R_scenario_flip <- sapply(R_scenario, flip_reverse)
R_combo_df <- cbind(R_scenario, R_scenario)

# We obtain similar results when drawing R from a distribution that reflect VA's scheme:
# VAlow ~ exp
# VAmed ~ normal
# VAhigh ~ left

# Because VA did not allow for variance among species' Rs (either within or across guilds), 
# this *perhaps?* indicates that variation AMONG species is not important.
# That is, it doesn't matter whether ONE species has a high R, or they ALL do.
# Thus, even a few obligate mutualists make a network more prone to an extinction cascade.
# W



# 3  VA_high  VA_low
R_symmetrical <- R_options_full[
							apply(
								X = R_options_full[,2:3], 
								MARGIN = 1, 
								FUN = function(x) identical(as.character(x[1]), as.character(x[2]))
								),
							]

# alternate
R_fixed <- data.frame(
	scenario = R_options, 
	row_R = R_options,
	col_R = R_options
)

R_scenarios_assymetrical <- c(
	"VA_high-VA_low", 
	"exp-VA_low", 
	"power-VA_low", 
	"unif-VA_low", 
	"normal-VA_low", 
	"left-VA_low", 
	"bimod-VA_low", 
	"exp-VA_high", 
	"power-VA_high", 
	"unif-VA_high", 
	"normal-VA_high", 
	"left-VA_high", 
	"bimod-VA_high", 
	"left-exp", 
	""
	)



# We found no evidence to suggest that the effect of assymmetry was guild-specific; that is, whether qualitatively different outcomes resulted depending on whether plants or pollinators were assigned higher R values. (Supplemental figures "cascade_by_R_flip.pdf")

R_assymetrical <- data.frame(
	scenario = 
	row_R = 
	col_R = 
	
)

?expand.grid


R_options_full[1,2:3][,2]


# set 
R_input <- # for all options, use: R_options_full

###########################################################
#------------- RUN THE SIMULATIONS
###########################################################


N_iterations <- 1


R_combo_out <- list()

for(Rcombo in 1:nrow(R_input)){
	current_Rrows <- as.character(R_input[Rcombo,"row_R"])
	current_Rcols <- as.character(R_input[Rcombo,"col_R"])
	
	# DO THE SIMULATIONS ON EACH NETWORK
	stored_events <- list()
	just_extinctions <- list()
	extinctions_by_guild <- list()
	total_extinctions <- list()
	total_cascades <- list()
	cascade_frequency <- list()
	
	for(i in 1:length(mat_empirical)){
	
		mat <- mat_empirical[[i]]
		
		# if(min(c(rowSums(mat), colSums(mat))) == 0){
			# print("Hey, you have a species in your network that does not interact with any other species. The following functions will get stuck.")
			# next()
		# }
		
		# m<-nrow(mat)
		# n<-ncol(mat)
		
		stored_events[[i]] <- extinction_cascader(imatrix = mat, R_row = current_Rrows, R_col = current_Rcols, nsims = N_iterations)
		
		just_extinctions[[i]] <- lapply(stored_events[[i]], "[", c("lost_cols", "lost_rows"))
		
		extinctions_by_guild[[i]] <- do.call(rbind, lapply(just_extinctions[[i]], lengths))
		
		total_extinctions[[i]] <- rowSums(extinctions_by_guild[[i]])
		
		total_cascades[[i]] <- sum(total_extinctions[[i]] > 1)
		
		cascade_frequency[[i]] <- total_cascades[[i]]/length(stored_events[[i]])
		
	}

	R_combo_out[[Rcombo]] <- list(stored_events = stored_events, just_extinctions = just_extinctions, extinctions_by_guild = extinctions_by_guild, total_extinctions = total_extinctions, total_cascades = total_cascades, cascade_frequency = cascade_frequency)

}
names(R_combo_out) <- R_scenario

# for 6 networks and 72 R combos, for 10000 iterations: took approximately 3 hrs on 2.3GHz*4 16GBRAM
# save(R_combo_out, file = "R_72_net_6_iter100.RData")
# save(R_combo_out, file = "R_combo_out2.RData")
lsos() # memory checking function



###########################################################
#------------- PROCESS THE OUTPUT
###########################################################

# R_combo_out
	# [[1:81 R_combo]]
	# [[1:6 
		# 1: stored_events, 
		# 2: just_extinctions, 
		# 3: extinctions_by_guild, 
		# 4: total_extinctions, 
		# 5: total_cascades, 
		# 6: cascade_frequency]]
	# [[1:N_networks]]
	# [[1:N_iterations]]


do.call(c, R_combo_out[[1]][["cascade_frequency"]])
length(R_combo_out[[1]][["stored_events"]][[1]])
lapply(R_combo_out[[1]][["stored_events"]][[1]], "[", c("interaction_matrix"))
do.call(c, R_combo_out[[1]][["cascade_frequency"]])





#--------------------PLOTTING-------------------------------

plot_list <- list()
for(i in 1:length(R_combo_out)){
    plot_list[[i]] <- do.call(c, R_combo_out[[i]][[6]])
    names(plot_list)[i] <- R_scenario[i]  
}
# names(R_combo_out[[1]])


pdf(file = file.path(fig_dir, "cascade_freq_by_R_full.pdf"), width = 12, height = 7)
batches <- list(1:9, 10:18, 19:27, 28:36, 37:45, 46:54, 55:63, 64:72, 73:81)
for(batch in 1:length(batches)){
  set.seed(1)
  par(mar = c(5,4,1,1))
  stripchart(
    x = plot_list[batches[[batch]]], 
    vertical = TRUE,
    method = "jitter",
    jitter = 0.2,
    pch = 1, col = "black", #bg = "grey",
    cex = 0.8,
    # las = 2,
    ylab = paste("Cascade proportion per", N_iterations, "iterations"),
    ylim = c(0,1),
    las = 2, 
    xaxt = "n"
    # group.names = c("within tags,\nsingle PCR", "between tags,\nsingle PCR", "within tags,\ndouble PCR", "between tags,\ndouble PCR")
  )
    # add axis
  axis(
    side = 1,
    at = 1:length(plot_list[batches[[batch]]]),
    tick = TRUE,
    labels = FALSE, 
    line = 0
  )
  
  # add labels
  text(
    x = 1:length(plot_list[batches[[batch]]]), 
    y = -0.1, 
    labels = names(plot_list[batches[[batch]]]), 
    xpd = TRUE, 
    srt = 45, 
    pos = 2, 
    cex = 0.8
    )

}

dev.off()


# -----------------------------------------------------------
# subset by pairs of reciprocal/flipped/inverted R structures
pdf(file = "cascade_by_R_flip.pdf")
for(rownum in 1:nrow(R_combo_df)){
	plot_by_flip <- list(
		do.call(c, R_combo_out[[R_combo_df[rownum,1]]][[6]]), 
		do.call(c, R_combo_out[[R_combo_df[rownum,2]]][[6]])
		)
	names(plot_by_flip) <- R_combo_df[rownum,]
	  set.seed(1)
  par(mar = c(5,4,1,1))
  stripchart(
    x = plot_by_flip, 
    vertical = TRUE,
    method = "jitter",
    jitter = 0.2,
    pch = 1, col = "black", #bg = "grey",
    cex = 0.8,
    # las = 2,
    ylab = paste("Cascade proportion per", N_iterations, "iterations"),
    ylim = c(0,1),
    las = 2, 
    xaxt = "n"
    # group.names = c("within tags,\nsingle PCR", "between tags,\nsingle PCR", "within tags,\ndouble PCR", "between tags,\ndouble PCR")
  )
    # add axis
  axis(
    side = 1,
    at = 1:length(plot_by_flip),
    tick = TRUE,
    labels = FALSE, 
    line = 0
  )
  
  # add labels
  text(
    x = 1:length(plot_by_flip), 
    y = -0.1, 
    labels = names(plot_by_flip), 
    xpd = TRUE, 
    srt = 45, 
    pos = 2, 
    cex = 0.8
    )

}

dev.off()



#~~~~~~~~~~~~~~~~~~~~GRAVEYARD~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# freq_VAlow <- do.call(c, R_combo_out[["VA_lowVA_low"]][[6]])
# freq_VAmed <- do.call(c, R_combo_out[["VA_medVA_med"]][[6]])
# freq_VAhigh <- do.call(c, R_combo_out[["VA_highVA_high"]][[6]])
# freq_VAhilo <- do.call(c, R_combo_out[["VA_highVA_low"]][[6]])
# freq_leftleft <- do.call(c, R_combo_out[["leftleft"]][[6]])
# freq_unifunif <- do.call(c, R_combo_out[["unifunif"]][[6]])
