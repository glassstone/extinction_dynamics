#05-04-16- Mathias Pires Extinction simulation - netcascade
#Effect of each losing species
#==================================
rm(list=(ls()))
setwd("/Users/piresmm/Dropbox/NIMBioS Working Group/extinctions/Analysis")

data_dir <- file.path("..", "Data")
fig_dir <- file.path("..", "Figures")

correl=FALSE
positive=TRUE

#-----------------------------------------
#Load network
#-----------------------------------------

emp_nets<-dir(path = emp_net_path) #Plant-pollinator weighted nets
mat_empirical <- list()
for(p in 1:length(emp_nets)){
  mat_empirical[[p]] <- as.matrix(read.table(file.path(emp_net_path, emp_nets[p])))
}
mat <- mat_empirical[[p]]
m<-nrow(mat)
n<-ncol(mat)

k_rows<-apply(mat,1,sum) #species degrees
k_cols<-apply(mat,2,sum) #species degrees

#-----------------------------------------
#Define R distribution
#-----------------------------------------

#-----------------------------------------
#Reassigning R according to species degree
#-----------------------------------------
if (correl==TRUE){
  if (positive==TRUE){
    
    #positive correlation
    aux<-rank(k_rows, ties.method="first")
    temp<-sort(R_rows,decreasing=FALSE)
    R_rows<-temp[aux]
    
    aux<-rank(k_cols, ties.method="first")
    temp<-sort(R_cols,decreasing=FALSE)
    R_cols<-temp[aux]
  }else{
    #negative correlation
    aux<-rank(k_rows, ties.method="first")
    temp<-sort(R_rows,decreasing=TRUE)
    R_rows<-temp[aux]
    
    aux<-rank(k_cols, ties.method="first")
    temp<-sort(R_cols,decreasing=TRUE)
    R_cols<-temp[aux]
  }
}

#-------------
#Netcascade
#-------------

guilds=c("rows","cols")
richness<-c(m,n)
casc_deg_list=list()
casc_num_list=list()

for (1 in 1:2){
  guild<-guilds[i]
  S<-richness[i]
  casc_deg<-matrix(NA,nsims,S) #each sim in a row, each species in a column
  casc_num<-matrix(NA,nsims,S)
  for (j in 1:S){
    target<-j
    netcascade #runnetcascade
    casc_deg[,j]<-
    casc_num[,j]<-
  }
  casc_deg_list[[i]]<-casc_deg
  casc_num_list[[i]]<-casc_num
}