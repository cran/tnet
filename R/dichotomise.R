`dichotomise` <-
function(edgelist,GT=0){
  edgelist <- as.matrix(edgelist)
  #Extract ties with a weight greater than GT
  edgelist <- edgelist[edgelist[,3]>GT,]
  #Set their weight to 1
  edgelist[,3] <- 1
  row.names(edgelist)<-NULL
  #Assign names to edgelist
  dimnames(edgelist)[[2]]<-c("i","j","w")
  return(edgelist)
}