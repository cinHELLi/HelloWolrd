#The code refers to Cinelli et al., Structural bounds on the dyadic effect. Journal of Complex Networks (2017)

#n1 is the number of nodes with c_i=1
#N is the number of network nodes
#M is the number of links of the network
#deg_seq is the vector of node degree arranged in non-increasing order

m11.max.new <- function(n1,M,deg_seq){ 
  ds1 <- deg_seq[1:n1]
  ds1[ds1 > (n1-1)] <- n1-1 
  min(M,choose(n1,2),ceiling(sum(ds1)/2))
}
m10.max.new <- function(n1,N,M,deg_seq){
  ds1 <- deg_seq[1:n1]
  ds1[ds1 > (N - n1)] <- N - n1
  ds0 <- deg_seq[1:(N - n1)]
  ds0[ds0 > n1] <- n1
  min(M,n1*(N-n1),min(sum(ds1),sum(ds0)))
}

m11.min.new <- function(n1,N,M,deg_seq){
  if(n1!=N){
    max(0,floor((sum(deg_seq[N:(N-n1+1)])-sum(deg_seq[1:(N-n1)]))/2))#il +1 è per la vettorizzazione
  }else(M)
}

m10.min.new <- function(n1,N,deg_seq){
  if(n1 != 0 & n1 != N ){
    somma <- sum(deg_seq[N:(N-n1+1)])-n1*(n1-1)
  } else {somma = 0}
  max(1,somma)
}
