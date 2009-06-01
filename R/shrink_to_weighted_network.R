`shrink_to_weighted_network` <-
function(edgelist){
  edgelist <- as.data.frame(edgelist)
  #Order edgelist
  edgelist <- edgelist[order(edgelist[,1],edgelist[,2]),]
  #Find duplicates
  edgelist[,3] <- as.numeric(!duplicated(edgelist[,1:2]))
  #Create an index of ties
  edgelist[,4] <- cumsum(edgelist[,3])
  #Count duplications
  edgelist[edgelist[,3]==1,5] <- tapply(edgelist[,3], 
    edgelist[,4], length)
  #Extract relevant columns
  edgelist <- edgelist[edgelist[,3]==1,c(1,2,5)]
  row.names(edgelist)<-NULL
  #Assign names to columns
  dimnames(edgelist)[[2]]<-c("i","j","w")
  return(edgelist)
}