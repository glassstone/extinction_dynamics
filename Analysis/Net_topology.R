
library(tnet)
library(bipartite)
setwd("/Users/piresmm/Dropbox/NIMBioS Working Group/extinctions/Data")
nam<-dir() #Plant-pollinator weighted nets

#Loading net
p<-1
mat<-as.matrix(read.table(nam[p])) #importing matrix
dimnames(mat)=NULL

#Dimensions
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

#=========================
#Network level properties
#=========================

#................
#dependence asymmetry
#................

int.pos<-which(mat>0)
aux<-dep.m[,,1]; dep1<-aux[int.pos]
aux<-dep.m[,,2]; dep2<-aux[int.pos]
plot(dep1~dep2)
abline(lm(dep1~dep2))
summary(lm(dep1~dep2))

#...............................
#threshold strucutural analysis
#...............................

Nest=c(NA)
Connect=c(NA)
vec<-seq(0,0.1,by=0.05) #threshold vector

#!!!!Check matrix transformation!!!
for (i in 1:length(vec)){
  mat.aux<-mat/sum(mat)
  mat.aux[mat.aux>vec[i]]=1
  mat.aux[mat.aux<vec[i]]=0
  
  #removing disconnected spp
  zero.r<-which(apply(mat.aux,1,sum)==0)
  zero.c<-which(apply(mat.aux,2,sum)==0)
  if(length(zero.r>0)){mat.aux<-mat.aux[-zero.r,]}
  if(length(zero.c>0)){mat.aux<-mat.aux[,-zero.c]}
  
  #computing nestedness, connectance, clustering
  Nest[i]<-nested(mat.aux, method="NODF2")
  Connect[i]<-sum(mat.aux)/(nrow(mat.aux)*ncol(mat.aux))
  clust[i]<-clustering_tm(T[,1:2]) #(Opsahl 2010)
 
}


#................
#Species strenth
#................

#Animal strength 
A.str<-apply(dep.m[,,1],1, sum)
#Plant strength 
P.str<-apply(dep.m[,,2],2, sum)
hist(P.str)

#................
#Normalised degree
#................
ND(mat)

#................
#weigthed degree (Opsahl et al. 2007)
#................

T=web2edges(mat,both.direction=TRUE, return=TRUE) #edge list

degrees<-degree_w(T,measure=c("degree", "output", "alpha"),alpha=0.5) 

#plotting
par(mfrow=c(2,1), mar=c(4,3,3,3))
hist(degrees[1:A,2], main="Rows",xlab="k")
hist(degrees[(A+1):(P+A),2], main="Columns",xlab="k")

par(mfrow=c(2,1), mar=c(4,3,3,3))
hist(degrees[1:A,4], main="Rows",xlab="wk")
hist(degrees[(A+1):(P+A),4], main="Columns",xlab="wk")

#................................
#species dependencies distribution
#maybe we can use blutghen index
#.................................


#................................
#Pairwise distances
#We may want to see how the distribution of pairwise distances each sp changed as well
#This is the metric for pw distance in a weighte net
#.................................
distance_w(T, directed=FALSE)


#Bipartite has a function that computes all metrics now...
networklevel(mat)

