#This function return a matrix containing all the possible binary vectors
#the number of rows is N
#the number of colums is choose(N,n1) 

f <- function(N=5,n1=3) 
  combn(1:N,n1,FUN=function(cm) replace(rep(0,N),cm,1))

#This procedure returns the matrix of the "phase diagram"
#the result can be plotted using, for instance, the library(lattice) in R
#the library(igraph) is required in order to run complete_enumeration_fast

#g is the network it has to be UNDIRECTED and UNWEIGHTED

#CombAtt is the result of the function f

complete_enumeration_fast <- function (g, CombAtt) 
{
  if (!is.igraph(g)) {
    stop(sprintf("%s is not a graph object", deparse(substitute(g))))
  }
  
  M <- ecount(g)
  vect_n1 <- colSums(CombAtt)
  Count <- matrix(0,nrow = (M+1), ncol= (M+1))
  
  for(i in 1:ncol(CombAtt)){
    
    caratteristiche <- CombAtt[,i]
    n0 <- which(caratteristiche==0)
    m00_subgraph <- induced.subgraph(g, n0)
    m00 <- ecount(m00_subgraph)
    
    n1 <- which(caratteristiche==1)
    m11_subgraph <- induced.subgraph(g, n1)
    m11 <- ecount(m11_subgraph)
    
    m10 <- ecount(g) - m11 - m00
    
    Count[m11+1,m10+1] <- Count[m11+1,m10+1]+1
    rownames(Count) <- c(0:M)
    colnames(Count) <- c(0:M)
    
  }
  Count <- Count[nrow(Count):1,]
  
  return(Count)
}  
