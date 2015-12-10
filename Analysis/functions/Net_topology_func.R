
#Analysis of the topology of networks after extinction cascades 
#(We need to recover the nets after extinctions)
#analysis of the topological role of exinct and extant species after cascades 
#(We need to recover a list of extinct and a list of extant species after cascades)
#=============================================================================

net.topology <-function(mat=NULL){  
  
  library(tnet)
  library(bipartite)
  out<-list()

  #Matrix Dimensions
  dimnames(mat)<-NULL
  A<-nrow(mat) #animals = rows
  P<-ncol(mat) #plants = columns

  #Dependence matrices
  dep.m <- array(0,dim=c(A,P,2))
  for(i in 1:P){
    dep.m[,i,1] <- mat[,i]/sum(mat[,i])
  } #matrix of plant dependence on each animal
  for(i in 1:A){
    dep.m[i,,2] <- mat[i,]/sum(mat[i,])
  } #matrix of animal dependence on each plant


  #................
  #dependence asymmetry
  #................

  int.pos<-which(mat>0)
  aux<-dep.m[,,1]; dep1<-aux[int.pos]
  aux<-dep.m[,,2]; dep2<-aux[int.pos]
  symmetry<-cor.test(dep1,dep2)

  #...............................
  #threshold strucutural analysis
  #...............................

  Nest<-c(NA)
  Connect<-c(NA)

  #list of all non-zero elements of the matrix ordered by weight
  nonzero.values<-c(min(which(sort(mat)!=0)),max(which(sort(mat)!=0)))
  nonzero.list<-order(mat)[nonzero.values[1]:nonzero.values[2]] 

  #Remove 5% smallest values, 10%...
  rem.percentage<-seq(0.05,0.75,by=0.05)

  for (i in 1:length(rem.percentage)){
    remove<-round(rem.percentage[i]*length(nonzero.list))
    nonzero.r<-nonzero.list[-(1:remove)]
    mat.aux<-matrix(0,A,P)
    mat.aux[nonzero.r]<-1
  

    #removing disconnected spp
    zero.r<-which(apply(mat.aux,1,sum)==0)
    zero.c<-which(apply(mat.aux,2,sum)==0)
    if(length(zero.r>0)){mat.aux<-mat.aux[-zero.r,]}
    if(length(zero.c>0)){mat.aux<-mat.aux[,-zero.c]}
  
    #computing nestedness, connectance
    Nest[i]<-nested(mat.aux, method="NODF2")
    Connect[i]<-sum(mat.aux)/(nrow(mat.aux)*ncol(mat.aux))
  }


  #..............................
  #Species strength distribution
  #.............................

  #Animal strength 
  A.str<-apply(dep.m[,,1],1, sum)
  #Plant strength 
  P.str<-apply(dep.m[,,2],2, sum)
  #................
  #Normalized degree
  #................
  Norm.degree<-ND(mat)

  #................
  #weigthed degree (Opsahl et al. 2007)
  #................

  T=web2edges(mat,both.direction=TRUE, return=TRUE) #edge list

  degrees<-degree_w(T,measure=c("degree", "output", "alpha"),alpha=0.5) 

  #................................
  #Pairwise distances
  #We may want to see how the distribution of pairwise distances each sp changed as well
  #This is the metric for pw distance in a weighted net
  #.................................
  pw_dist<-distance_w(T, directed=FALSE)
  mean(pw_dist, na.rm=TRUE)
  hist(as.numeric(pw_dist))

  #===================================
  #Storing in a list - function output
  #===================================
  out[[1]]<-dep.m
  out[[2]]<-symmetry
  out[[3]]<-Nest
  out[[4]]<-Connect
  out[[5]]<-A.str
  out[[6]]<-P.str
  out[[7]]<-Norm.degree
  out[[8]]<-degrees
  out[[9]]<-pw_dist
  names(out)=c("dependence","dep_symmetry","Nestedness","Connectance","A.strength","P.strength","Normalized_degree","weigthed_degree","pairwise_distance")

  return(out)
}

