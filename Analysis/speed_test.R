
N_iterations <- 10000

netcascade_out_loop <- list()

system.time(
for(i in 1:N_iterations){
  netcascade_out_loop[[i]] <- netcascade_JO(
    intmat, 
    R_rows = rep(1, each = nrow(intmat)), 
    R_cols = rep(0.5, each = ncol(intmat)), 
    unluckyGuild = "rows", 
    unluckySpecies = 2
    )
}
)

system.time(
  netcascade_out_rep <- replicate(N_iterations, netcascade_JO(
    intmat, 
    R_rows = rep(1, each = nrow(intmat)), 
    R_cols = rep(0.5, each = ncol(intmat)), 
    unluckyGuild = "rows", 
    unluckySpecies = 2
  ), simplify = FALSE)
)


system.time(
  means <- replicate(100000, mean(rnorm(50)))
)

means <- c()
system.time(
  for(i in 1:100000) { 
    means <- c(means, mean(rnorm(50)))
  }
)

