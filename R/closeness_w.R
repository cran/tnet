`closeness_w` <-
function(net,directed=NULL,precomp.dist=NULL){
  # Ensure that the network conforms to the tnet standard
  if (is.null(attributes(net)$tnet))                      net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet")   stop("Network not loaded properly")

  # Compute distance matrix                
  if(is.null(precomp.dist)) {
    # Check if network is directed
    if(is.null(directed)&is.null(precomp.dist))
      directed   <- (nrow(symmetrise(net))!=nrow(net)) 
    precomp.dist <- distance_w(net=net, directed=directed)
  }
  # Sum up distances to all other nodes to get farness
  out <- cbind(node=attributes(precomp.dist)$nodes, closeness=rowSums(precomp.dist, na.rm=TRUE))
  # Invert to get closeness
  out[,"closeness"] <- 1/out[,"closeness"]
  return(out)
}