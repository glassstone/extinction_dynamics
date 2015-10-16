#=====================================================================
#09-01-2015 - Extinction Nimbios group
#Exploring the stochastic coextinction model (Vieira and Almeida Neto 2015 - Ecol Lett)
#=============================================================================

# Set path to the project folder:
# MMP: project_dir <- "/Users/piresmm/Dropbox/NIMBioS Working Group/extinctions"
# JO:  project_dir <- "/Users/jimmy.odonnell/Projects/extinction_dynamics"

setwd(project_dir)

# load original functions (everything in the folder "Analysis/functions")
sapply(dir(path = "./Analysis/functions", full.names = TRUE), source)

#..............................
#Testing functions with a sample matrix
m<-10
n<-5
mat<-matrix(rbinom(100,10,0.1),m,n) #random weighted network

#extinction degree (the number of times one extinction spreads to the other set of species)
coextDeg(mat,0.9,1,100)

#number of coextinction
coextNumber(mat,0.1,0.4,100)
#..............................


#============================
#Ideas
#=============================

#First test - How including variable Rvalues (species dependence on interactions) across species change the original results? 
#Use the same network, same rlow, same rup, 
#one set of simulations with one Rvalue (as in Vieira and Almeida-Neto 2015)
#second set of simulation with several Rvalues



#Second test - How different distributions of Rvalues (different ways in which species depend on the mutualism) 
#change the probability of cascades and the number of coextinctions?
#We could choose uniform, normal and exponential, and few parameter combinations for each (3?)


#Third test - Does network topology affect the consequence of multiple Rvalues?
#Perform a series of tests with networks with different topologies

#for all tests besides looking at the number of coextinctions and coextinctions degree we could examine how topology changed

