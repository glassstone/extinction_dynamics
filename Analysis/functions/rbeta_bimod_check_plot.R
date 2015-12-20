# plot to check
N_draws <- 1000000
left_shape1 <- 0.8
left_shape2 <- 2
right_shape1 <- 13
right_shape2 <= 4
hist(
  rbeta_bimod(n = N_draws, 
	beta1shape1 = left_shape1, 
	beta1shape2 = left_shape2, 
	beta2shape1 = right_shape1, 
	beta2shape2 = right_shape2
        ), 
  col = rgb(1, 0, 0, 0.2), 
  breaks = seq(from = 0, to = 1, by = 0.01), 
  prob = TRUE, 
  xlim = c(0,1), 
  main = "Bimodal distribution for R", 
  xlab = expression(italic("R"))
  )
mtext(
	text = paste("N =", N_draws), 
	side = 3, 
	line = 0
)

# after Vogler and Kalisz 2001
N_draws <- 170
hist(
  rbeta_bimod(n = N_draws, 
	beta1shape1 = 0.8, 
	beta1shape2 = 2, 
	beta2shape1 = 13, 
	beta2shape2 = 4
        ), 
  col = rgb(1, 0, 0, 0.2), 
  breaks = seq(from = 0, to = 1, by = 0.05), 
  prob = FALSE, 
  # xlim = c(0,1), 
  main = "Bimodal distribution for R", 
  xlab = expression(italic("R"))
  )
mtext(
	text = paste("N =", N_draws), 
	side = 3, 
	line = 0
)

