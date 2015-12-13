###########################################################
# TEST TO BE SURE ALL THE FUNCTIONS WORK THE SAME
###########################################################



#----------------------------------------------------------
# VIEIRA
#----------------------------------------------------------
extinction_number_Vieira <- list()
for(i in 1:length(mat_empirical)){
	extinction_number_Vieira[[i]] <- coextNumber_Vieira(mat_empirical[[i]], rlow = 0, rup = 0.3, nsims = 100)
}
Vieira_low <- do.call(c, lapply(extinction_number_Vieira, function(x) sum(x > 1)))/100

extinction_number_Vieira <- list()
for(i in 1:length(mat_empirical)){
	extinction_number_Vieira[[i]] <- coextNumber_Vieira(mat_empirical[[i]], rlow = 0.3, rup = 0.6, nsims = 100)
}
Vieira_med <- do.call(c, lapply(extinction_number_Vieira, function(x) sum(x > 1)))/100

extinction_number_Vieira <- list()
for(i in 1:length(mat_empirical)){
	extinction_number_Vieira[[i]] <- coextNumber_Vieira(mat_empirical[[i]], rlow = 0.6, rup = 1, nsims = 100)
}
Vieira_high <- do.call(c, lapply(extinction_number_Vieira, function(x) sum(x > 1)))/100

set.seed(1)
par(mar = c(5,4,1,1))
stripchart(
  x = list(Vieira_low, Vieira_med, Vieira_high), 
  vertical = TRUE,
  method = "jitter",
  jitter = 0.2,
  pch = 1, col = "black", #bg = "grey",
  cex = 1,
  # las = 2,
  ylab = "Cascade frequency",
  ylim = c(0,1),
  # xaxt = "n", 
  group.names = c("R{0,0.3}", "R{0.3,0.6}", "R{0.6,1}")
)

#----------------------------------------------------------
# JIMMY'S HOMEBREW (netcascade_JO), using Vieira's values
#----------------------------------------------------------
extinction_number_JO <- list()
for(i in 1:length(mat_empirical)){
	extinction_number_JO[[i]] <- coextNumber_JO(mat_empirical[[i]], rlow = 0, rup = 0.3, nsims = 100)
}
JO_low <- do.call(c, lapply(extinction_number_JO, function(x) sum(x > 1)))/100

extinction_number_JO <- list()
for(i in 1:length(mat_empirical)){
	extinction_number_JO[[i]] <- coextNumber_JO(mat_empirical[[i]], rlow = 0.3, rup = 0.6, nsims = 100)
}
JO_med <- do.call(c, lapply(extinction_number_JO, function(x) sum(x > 1)))/100

extinction_number_JO <- list()
for(i in 1:length(mat_empirical)){
	extinction_number_JO[[i]] <- coextNumber_JO(mat_empirical[[i]], rlow = 0.6, rup = 1, nsims = 100)
}
JO_high <- do.call(c, lapply(extinction_number_JO, function(x) sum(x > 1)))/100

set.seed(1)
par(mar = c(5,4,1,1))
stripchart(
  x = list(JO_low, JO_med, JO_high), 
  vertical = TRUE,
  method = "jitter",
  jitter = 0.2,
  pch = 1, col = "black", #bg = "grey",
  cex = 1,
  # las = 2,
  ylab = "Cascade frequency",
  ylim = c(0,1),
  # xaxt = "n", 
  group.names = c("R{0,0.3}", "R{0.3,0.6}", "R{0.6,1}")
)

# This works!

#----------------------------------------------------------
# JIMMY'S HOMEBREW, using values drawn from beta
#----------------------------------------------------------

extinction_number_JO <- list()
for(i in 1:length(mat_empirical)){
	extinction_number_JO[[i]] <- coextNumber_beta(mat_empirical[[i]], rlow = 0, rup = 0.3, nsims = 100)
}
beta_exp <- do.call(c, lapply(extinction_number_JO, function(x) sum(x > 1)))/100

extinction_number_JO <- list()
for(i in 1:length(mat_empirical)){
	extinction_number_JO[[i]] <- coextNumber_beta(mat_empirical[[i]], rlow = 0.3, rup = 0.6, nsims = 100)
}
beta_left <- do.call(c, lapply(extinction_number_JO, function(x) sum(x > 1)))/100

extinction_number_JO <- list()
for(i in 1:length(mat_empirical)){
	extinction_number_JO[[i]] <- coextNumber_beta(mat_empirical[[i]], rlow = 0.6, rup = 1, nsims = 100)
}
beta_unif <- do.call(c, lapply(extinction_number_JO, function(x) sum(x > 1)))/100

