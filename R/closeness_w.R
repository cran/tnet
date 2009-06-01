`closeness_w` <-
function(edgelist,directed=NULL,precomp.dist=NULL){
  edgelist     <- as.data.frame(edgelist)
  dimnames(edgelist)[[2]] <- c("i","j","w")
  #no self-loops
  edgelist     <- edgelist[edgelist[,"i"]!=edgelist[,"j"],]; 
  #all positive weights
  edgelist     <- edgelist[edgelist[,"w"]>0,];               
  #nodes
  N            <- max(edgelist[,c("i","j")])
  #check if network is directed
  if(is.null(directed)&is.null(precomp.dist))
    directed   <- (nrow(symmetrise(edgelist))!=nrow(edgelist)) 
  #compute distance matrix                
  if(is.null(precomp.dist)) {
    precomp.dist <- 
      distance_w(edgelist=edgelist, directed=directed)
  } else {
    if(nrow(precomp.dist) != N | ncol(precomp.dist) != N)
      stop("precomp.dist does not have the right dimensions")
  }
  #sum up distances to all other nodes
  res <- cbind(vertex=1:N, 
               closeness=rowSums(precomp.dist, na.rm=TRUE))
  res[,"closeness"] <- 1/res[,"closeness"]
  return(res)
}