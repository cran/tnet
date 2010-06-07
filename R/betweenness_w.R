`betweenness_w` <-
function(net, directed = NULL, alpha=1){
  # Ensure that the network conforms to the tnet standard
  if (is.null(attributes(net)$tnet))                      net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet")   stop("Network not loaded properly")

  # Check if network is directed
  if(is.null(directed)) 
    directed <- (nrow(symmetrise(net)) != nrow(net))
  
  # Load, prepare for, and use igraph
  library(igraph)
  net[,c("i","j")] <- net[,c("i","j")]-1
  net[,"w"] <- (1/net[,"w"])^alpha
  if(directed) {
    g <- graph.edgelist(el=as.matrix(net[,c("i","j")]), directed=TRUE)
    g <- set.edge.attribute(g, "weight", value=net[,"w"])
  } else {
    g <- graph.edgelist(el=as.matrix(net[net[,"i"]<net[,"j"],c("i","j")]), directed=FALSE)
    g <- set.edge.attribute(g, "weight", value=net[net[,"i"]<net[,"j"],"w"])
  }
  N <- length(V(g))
  out <- cbind(node = 1:N, betweenness = 0)
  for(i in 0:(N-1)) {
    if(directed) {
      paths <- get.shortest.paths(g, from=i)
    } else {
      paths <- get.shortest.paths(g, from=i, to=V(g)[V(g)>i])
    }
    tmp <- which(sapply(paths, length)>2)
    if(length(tmp)>0) {
      paths <- paths[tmp]
      for(j in 1:length(paths))
        paths[[j]] <- paths[[j]][2:(length(paths[[j]])-1)]
      paths <- unlist(paths)+1
      for(j in paths)
        out[j,"betweenness"] <- out[j,"betweenness"]+1
    }
  }
  return(out)                       
}

