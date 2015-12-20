# Note that this will only work if you are equally likely to pull events from each of the two underlying beta distributions (i.e. binomial probability = 0.5)
# doesn't work for odd numbers of draws
rbeta_bimod <- function(
	n, 
	beta1shape1, 
	beta1shape2, 
	beta2shape1, 
	beta2shape2
	){
	# bins <- rbinom(n = n, size = 1, prob = 0.5)
	nsplit <- c(round(n/2), n - round(n/2))
	betadraws1 <- rbeta(n = nsplit[1], shape1 = beta1shape1, shape2 = beta1shape2)
	betadraws2 <- rbeta(n = nsplit[2], shape1 = beta2shape1, shape2 = beta2shape2)
	betadrawsboth <- c(betadraws1, betadraws2)
	rbeta_bimod_out <- sample(x = betadrawsboth, size = n, replace = FALSE, prob = NULL)
	return(rbeta_bimod_out)
	}

