#!/usr/bin/env Rscript

# Some options to create beta distributions similar to other scenarios
#===============================================================================

# start with a temp matrix just to make code readable and easy to add options
temp <- matrix(data = c(
	"unif",           1.0, 1.0,
	"normal",         3.0, 3.0,
	"bimod",          0.2, 0.2,
	"powerL",          1.0, 0.5,
	"powerR",          0.5, 1.0,
	"left",           4.0, 0.1,
	"right",          0.1, 4.0,
	"Vogler2001",      NA,  NA,
	"Vogler2001_L",   0.8, 2.0,
	"Vogler2001_R",  13.0, 4.0
	), ncol = 3, byrow = TRUE
)

beta_par_R <- data.frame(
	name = as.character(temp[,1]), 
	par1 = as.numeric(temp[,2]), 
	par2 = as.numeric(temp[,3]),
	stringsAsFactors = FALSE
)

rm(temp)