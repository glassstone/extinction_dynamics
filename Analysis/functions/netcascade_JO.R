#(December 8 2015).

netcascadeJO <- function(
					imatrix, 
					R_rows, 
					R_cols, 
					unluckyGuild, 
					unluckySpecies, 
					extinct_cols=NULL, 
					extinct_rows=NULL, 
					return.matrix=FALSE
					){
  
  #---------ARGUMENT CHECKS-----------------------------------
  
  if(class(imatrix)!="matrix" || (nrow(imatrix)+ncol(imatrix))<3){stop("
  	'imatrix' must be an object of class 'matrix', with at least three species overall
  	")}
  	
  if(class(R_rows)!="numeric" || class(R_cols) != "numeric" || max(c(max(R_rows),max(R_cols)))>1 || min(c(min(R_rows),min(R_cols)))<0){stop("
  	'R_rows' & 'R_cols' must be numeric vectors with values ranging between 0 and 1
  	")}
  	
  if(length(R_rows)!= nrow(imatrix)){stop("
  	The length of vector'R_rows' must be equal to number of rows (i.e. species in guild) in 'imatrix'
  	")}
  	
  if(length(R_cols)!= ncol(imatrix)){stop("
  	The length of vector'R_cols' must be equal to number of columns in 'imatrix'
  	")}
  
  if((unluckyGuild%in%c("rows","cols"))==FALSE){stop('
  	Invalid target guild for primary extinction. Valid targets guilds are "rows" and "cols"
  	')}
  
  if(is.numeric(unluckySpecies)==FALSE){stop('
  	Invalid value for the "unluckySpecies" argument. You may specify a single species by entering its row or column number or you may use a vector of relative probabilites for all species in the unlucky guild.
  	')}
  
  if(is.null(extinct_cols)==FALSE && class(extinct_cols)!= "integer"){stop("
  	extinct_cols must be either NULL an integer vector specifying the column numbers of species considered to be extinct on the original matrix
  	")}
  	
  if(is.null(extinct_rows)==FALSE && class(extinct_rows)!= "integer"){stop("
  	extinct_rows must be either NULL or an integer vector specifying the row numbers of species considered to be extinct on the original matrix
  	")}
  	
  #---------DEFINING SOME VARIABLES---------------------------
  nrows <- nrow(imatrix)
  ncols <- ncol(imatrix)
  cols <- 1:ncols
  rows <- 1:nrows
  colsNA <- 1:ncols
  rowsNA <- 1:nrows
  colsNA[extinct_cols] <- NA
  rowsNA[extinct_rows] <- NA
  
  degree_when_lost_cols <- c()
  degree_when_lost_rows <- c()
  
  #----------CALCULATING DEPENDENCE MATRICES-------------------
  M <- array(0,dim=c(nrows,ncols,2))
  for(i in 1:ncols){
    M[,i,1] <- imatrix[,i]/sum(imatrix[,i])
  } #matrix of columns' dependence on each row
  for(i in 1:nrows){
    M[i,,2] <- imatrix[i,]/sum(imatrix[i,])
  } #matrix of rows' dependence on each column
  
  #-----------CHOOSING TARGET SPECIES FOR PRIMARY EXTINCTION---
  coext_rows <- c()
  coext_cols <- c()
  if(length(unluckySpecies)==1){
    if(unluckyGuild=="rows"){
      if(unluckySpecies %in% extinct_rows){stop('Specified target species for the primary extinction is already extinct')}
      coext_rows <- unluckySpecies
      degree_when_lost_rows <- 1 #stores the degree of the extinction event of every row lost during the coextinction cascade. 
    }
    if(unluckyGuild=="cols"){
      if(unluckySpecies %in% extinct_cols){stop('Specified target species for the primary extinction is already extinct')}
      coext_cols <- unluckySpecies
      degree_when_lost_cols <- 1
    }
  }else{
    nspecies <- switch(unluckyGuild, rows = nrows, cols = ncols)
    if(length(unluckySpecies)==nspecies){
      if(unluckyGuild =="rows"){
        alive <- rows[is.na(rowsNA)==FALSE]
        coext_rows <- sample(c(alive,0),1,prob = c(unluckySpecies[is.na(rowsNA)==FALSE],0))
        degree_when_lost_rows <- 1
      }
      if(unluckyGuild =="cols"){
        alive <- cols[is.na(colsNA)==FALSE]
        coext_cols <- sample(c(alive,0),1,prob = c(unluckySpecies[is.na(colsNA)==FALSE],0))
        degree_when_lost_cols <- 1
      }
    }else{
      stop('Length of "unluckySpecies" must be 1 (specifying a single species within the unlucky guild) or else be equal to the number of species in the unlucky guild (specifying probabilities of primary extinction for each species in the unlucky guild)')
    }
  }
  imatrix[coext_rows,] <- 0 
  imatrix[,coext_cols] <- 0
  
  lost_rows <- coext_rows #final list of rows which were "alive" in the original community but became extinct during this primary extinction + extinction cascade
  lost_cols <- coext_cols 
  
  #-------------------CASCADE LOOP---------------------------
  equilibrium <- FALSE
  degree <- 1
  degree_table <- data.frame(degree,guild=factor(unluckyGuild,levels=c("rows","cols")),n_extinctions=1)
  while(equilibrium == FALSE){
    extinct_rows <- coext_rows
    extinct_cols <- coext_cols
    colsNA[extinct_cols] <- NA
    rowsNA[extinct_rows] <- NA
    
    remaining_rows <- rows[is.na(rowsNA) == FALSE]
    remaining_cols <- cols[is.na(colsNA) == FALSE]
    
    # If one of the rows is extinct...
    if(length(extinct_rows)>0){
      for(i in 1:length(extinct_rows)){
      	# This is the first place R is used; modify accordingly, e.g.
      	# R_cols
      	# ...Cull a column if its value of P is greater than that of the remaining columns (drawn from uniform dist?)
        unlucky <- R_cols[remaining_cols]*M[extinct_rows[i],remaining_cols,1] > runif(length(remaining_cols))
        coext_cols = c(coext_cols, remaining_cols[unlucky])
      }
      coext_rows <- c()
      coext_cols <- unique(coext_cols)
      colsNA[coext_cols] <- NA
      lost_cols <- c(lost_cols,coext_cols)
      
      # remove all of that column's interactions
      imatrix[,coext_cols] <- 0
      # make its interaction strengths zero; recalculate interaction strengths
      for(i in 1:ncols){
        if(sum(imatrix[,i])==0){
          M[,i,1] <- 0
        }else{
          M[,i,1] <- imatrix[,i]/sum(imatrix[,i])
        }
      }
      # add a degree if there were additional columns extinct (why just 1?)
      if(length(coext_cols)>0){
        degree <- degree + 1
        degree_when_lost_cols <- c(degree_when_lost_cols, rep(degree,length(coext_cols)))
        degree_table[degree,] <- data.frame(degree,"cols",length(coext_cols))
      }
    }else{
      for(i in 1:length(extinct_cols)){
        unlucky <- R_rows[remaining_rows]*M[remaining_rows,extinct_cols[i],2] > runif(length(remaining_rows))
        coext_rows <- c(coext_rows, remaining_rows[unlucky])
      }
      coext_cols = c();
      coext_rows <- unique(coext_rows)
      lost_rows <- c(lost_rows,coext_rows)
      rowsNA[coext_rows] <- NA
      imatrix[coext_rows,] <- 0
      for(i in 1:nrows){
        if(sum(imatrix[i,])==0){
          M[i,,2] <- 0
        }else{
          M[i,,2] <- imatrix[i,]/sum(imatrix[i,])
        }
      }
      if(length(coext_rows)>0){
        degree <- degree + 1
        degree_when_lost_rows <- c(degree_when_lost_rows, rep(degree,length(coext_rows)))
        degree_table[degree,] <- data.frame(degree,"rows",length(coext_rows))  
      }
    }
    equilibrium <- equilibrium + (length(coext_cols)+length(coext_rows))==0
  }
  
  #-------------------OUTPUT---------------------------
  
  if(return.matrix==TRUE){
    return(list(interaction_matrix = imatrix, lost_rows = lost_rows, lost_cols = lost_cols))  
  }else{
    if(length(lost_rows)>0){
      spp_data_rows <- data.frame(lost_rows = lost_rows, degree_of_extinction = degree_when_lost_rows)
    }else{
      spp_data_rows <- "No rows were lost"
    }
    if(length(lost_cols)>0){
      spp_data_cols <- data.frame(lost_col = lost_cols,degree_of_extinction=degree_when_lost_cols)
    }else{
      spp_data_cols <- "No columns were lost"
    }
    return(list(cascade_data = degree_table, rows_species_data = spp_data_rows , cols_species_data = spp_data_cols))
  }
}
