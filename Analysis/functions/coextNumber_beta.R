#coextNumber performs simulations (n = nsims) of a single episode of primary extinction and its possible associated coextinction cascade and creates a frequency distribution for the total number of extinctions for each episode.
#primary extinctions are uniform random among both groups.'r' is sampled from a interval (rlow, rup)

coextNumber_beta <- function(imatrix, nsims, beta_par1_T1 = 1, beta_par2_T1 = 1, beta_par1_T2 = 1, beta_par2_T2 = 1, guild=NULL,target=NULL){
  ext_counts <- c()
  for(sim in 1:nsims){
    R_rows <- rbeta(n = nrow(imatrix), shape1 = beta_par1_T1, shape2 = beta_par2_T1)
    R_cols <- rbeta(n = ncol(imatrix), shape1 = beta_par1_T2, shape2 = beta_par2_T2)
    if (is.null(guild)){ #ADDED
      guild <- sample(
        x = c('rows','cols'), 
        size = 1, 
        replace = FALSE, 
        prob = c(nrow(imatrix), ncol(imatrix))
      )
    }
    if (is.null(target)){ #ADDED
      if(guild=="rows"){
        target <- rep(1,nrow(imatrix)) #same probability of primary extinction for all spp
      }else{
        target <- rep(1,ncol(imatrix)) #same probability of primary extinction for all spp
      }
    }
    sim_results <- netcascade_JO(
    								imatrix = imatrix, 
    								R_rows = R_rows, 
    								R_cols = R_cols, 
    								unluckyGuild = guild, 
    								unluckySpecies = target, 
								return.matrix=FALSE
    								)
    ext_counts[sim] <- sum(sim_results[[1]]$n_extinctions)
  }
  return(ext_counts)
}

