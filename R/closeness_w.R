`closeness_w` <-
function(net,directed=NULL,precomp.dist=NULL){
  if(is.null(attributes(net)$tnet))
    net <- as.tnet(net, type="weighted one-mode tnet")
  if(attributes(net)$tnet!="weighted one-mode tnet")
    stop("Network not loaded properly")
  #nodes
  N            <- max(net[,c("i","j")])
  #check if network is directed
  if(is.null(directed)&is.null(precomp.dist))
    directed   <- (nrow(symmetrise(net))!=nrow(net)) 
  #compute distance matrix                
  if(is.null(precomp.dist)) {
    precomp.dist <- 
      distance_w(net=net, directed=directed)
  } else {
    if(nrow(precomp.dist) != N | ncol(precomp.dist) != N)
      stop("precomp.dist does not have the right dimensions")
  }
  #sum up distances to all other nodes
  res <- cbind(node=1:N, closeness=rowSums(precomp.dist, na.rm=TRUE))
  res[,"closeness"] <- 1/res[,"closeness"]
  return(res)
}