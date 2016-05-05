#!/usr/bin/env Rscript

prune_absent <- function(this_mat){
# this function will remove species that do not have any interactions
	this_mat <- this_mat[which(rowSums(this_mat) != 0) , ]
	this_mat <- this_mat[ , which(colSums(this_mat) != 0)]
	return(this_mat)
}
