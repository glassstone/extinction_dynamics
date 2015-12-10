#coextNumber performs simulations (n = nsims) of a single episode of primary extinction and its possible associated coextinction cascade and creates a frequency distribution for the total number of extinctions for each episode.
#primary extinctions are uniform random among both groups.'r' is sampled from a interval (rlow, rup)

coextNumber_JO <- function(imatrix,rlow,rup,nsims){
  ext_counts <- c()
  for(sim in 1:nsims){
    rvalue <- runif(1,rlow,rup)
    R_rows <- rep(rvalue, nrow(imatrix))
    R_cols <- rep(rvalue, ncol(imatrix))
    guild <- sample(c('rows','cols'),1,F,c(nrow(imatrix),ncol(imatrix)))
    if(guild=="rows"){
      target <- rep(1,nrow(imatrix))
    }else{
      target <- rep(1,ncol(imatrix))
    }    
    sim_results <- netcascade_JO(imatrix = imatrix, R_rows = R_rows, R_cols = R_cols, unluckyGuild = guild, unluckySpecies = target)
    ext_counts[sim] <- sum(sim_results[[1]]$n_extinctions)
  }
  return(ext_counts)
}
