MAT <- matrix(
			data = round(rnorm(n = 24, mean = 20, sd = 5)), 
			nrow = 4
)
imatrix <- MAT
R_rows <- runif(n = nrow(MAT))
R_cols <- runif(n = ncol(MAT))
unluckyGuild <- "rows"
unluckySpecies <- 2
extinct_cols=NULL 
extinct_rows=NULL
return.matrix=FALSE
