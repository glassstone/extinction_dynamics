

setwd("/Users/threeprime/Documents/Dropbox/NIMBioS END/extinctions/Analysis")

source("coextDeg_mod.R")
source("coextNumber_mod.R")

# p 152 of Bolstead pdf
# suggested beta distribution parameters:
# 0.1 and 1 gives exponential-like distribution;
# 0.5 and 1 gives Pareto-like ("power-law") distribution
# 1 and 1 give uniform-like distribution
# 3 and 3 give normal-like distribution

# let m be the number of species in Trophic level 1
m <- 10

# let n be the number of species in Trophic level 2
n <- 5

mat <- matrix(rbinom(100,10,0.1),m,n)

if(min(c(rowSums(mat), colSums(mat))) == 0)
	stop("Hey, you have a species in your network that does not interact with any other species. The following functions will get stuck.")

# 1 and 1 give uniform-like distribution
deg_1_1 <- coextDeg(imatrix = mat, nsims = 1000, beta_par1 = 1, beta_par2 = 1)
num_1_1 <- coextNumber(imatrix = mat, nsims = 1000, beta_par1 = 1, beta_par2 = 1)

# 0.1 and 1 gives exponential-like distribution;
deg_0.1_1 <- coextDeg(imatrix = mat, nsims = 1000, beta_par1 = 0.1, beta_par2 = 1)
num_0.1_1 <- coextNumber(imatrix = mat, nsims = 1000, beta_par1 = 0.1, beta_par2 = 1)

# 3 and 3 give normal-like distribution
deg_3_3 <- coextDeg(imatrix = mat, nsims = 1000, beta_par1 = 3, beta_par2 = 3)
num_3_3 <- coextNumber(imatrix = mat, nsims = 1000, beta_par1 = 3, beta_par2 = 3)

pdf(file = "R_from_beta.pdf")

par(mfrow = c(2,3))
hist(deg_1_1, xlab = "degree", main = "beta(1,1) 'uniform'", ylim = c(0,1000))
hist(deg_0.1_1, xlab = "degree", main = "beta(0.1,1) 'exponential'", ylim = c(0,1000))
hist(deg_3_3, xlab = "degree", main = "beta(3,3) 'normal'", ylim = c(0,1000))
hist(num_1_1, xlab = "number", main = "beta(1,1) 'uniform'", ylim = c(0,1000))
hist(num_0.1_1, xlab = "number", main = "beta(0.1,1) 'exponential'", ylim = c(0,1000))
hist(num_3_3, xlab = "number", main = "beta(3,3) 'normal'", ylim = c(0,1000))

dev.off()




coextDeg(mat,0.9,1,100) #extinction degree (the number of times one extinction spreads to the other set of species)


coextNumber(mat,0.1,0.4,100)#number of coextinction