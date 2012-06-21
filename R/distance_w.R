`distance_w` <-
function(net, directed = NULL, gconly = TRUE, subsample = 1, seed = NULL){
  # Check whether confirms to tnet standard
  if (is.null(attributes(net)$tnet)) 
    net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet") 
    stop("Network not loaded properly")
  
  # Set seed
  if (!is.null(seed)) 
    set.seed(as.integer(seed))
    
  # Normalise weightes
  net[, "w"] <- mean(net[, "w"])/net[, "w"]
  
  # Check if network is directed
  if(is.null(directed)) {
    tmp <- symmetrise_w(net, method = "MAX")
    directed <- (nrow(tmp) != nrow(net) | sum(tmp[,"w"]) != sum(net[,"w"]))
  }
  
  # Do computation in igraph
  library(igraph0)
  net[,c("i","j")] <- net[,c("i","j")]-1
  if(directed) {
    g <- igraph0::graph.edgelist(el=as.matrix(net[,c("i","j")]), directed=TRUE)
    g <- igraph0::set.edge.attribute(g, "tnetw", value=net[,"w"])
  } else {
    g <- igraph0::graph.edgelist(el=as.matrix(net[net[,"i"]<net[,"j"],c("i","j")]), directed=FALSE)
    g <- igraph0::set.edge.attribute(g, "tnetw", value=net[net[,"i"]<net[,"j"],"w"])
  }
  if(gconly) {
    gc <- igraph0::clusters(g, mode="strong")
    gc <- which(gc$membership==names(sort(-table(gc$membership)))[1])
    g <- igraph0::subgraph(g, gc-1)
  }
  d <- igraph0::shortest.paths(g, mode="out", weights=igraph0::get.edge.attribute(g, "tnetw"))
  diag(d) <- NA
  attributes(d)$nodes <- as.integer(V(g)+1)
  return(d)
}
