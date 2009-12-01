`distance_w` <-
function(net,directed=NULL,gconly=TRUE){
  # Ensure that the network conforms to the tnet standard
  if (is.null(attributes(net)$tnet))                      net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet")   stop("Network not loaded properly")

  # Invert tie weights to get cost
  net[,"w"] <- 1/net[,"w"]     
  # Check whether the net is directed        
  if(is.null(directed))
    directed <- (nrow(symmetrise(net))!=nrow(net))
  # Number of nodes
  n <- max(net[,c("i","j")])                 
  # Define distance matrix
  d <- matrix(data=Inf, ncol=n, nrow=n)   
  # Convert the data so that the RBGL package can read it
  library(RBGL);
  V <- as.character(1:n);
  E <- vector("list", length=n);
  names(E) <- V;
  for(i in 1:n)
    E[[i]] <- list(edges=net[net[,"i"]==i,"j"], weights=net[net[,"i"]==i,"w"])
  if(directed) {
    g <- new("graphNEL", nodes=V, edgeL=E, edgemode="directed")
  } else {
    g <- new("graphNEL", nodes=V, edgeL=E, edgemode="undirected")
  }
  # Extract giant component
  gc <- nodes(g)
  if(gconly) {
    gc <- connComp(g)
    gc <- gc[[which.max(sapply(gc,length))]]
    g <- subGraph(gc, g)
  }
  # Calculate the distances
  d <- t(sapply(gc, function(a) dijkstra.sp(g, start=a)$distances))
  diag(d) <- NA
  attributes(d)$nodes <- as.integer(gc)
  return(d)
}