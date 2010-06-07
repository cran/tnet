`distance_w` <-
function(net, directed = NULL, gconly = TRUE, subsample = 1, seed = NULL){
  if (is.null(attributes(net)$tnet)) 
    net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet") 
    stop("Network not loaded properly")
  if (!is.null(seed)) 
    set.seed(as.integer(seed))
  net[, "w"] <- mean(net[, "w"])/net[, "w"]
  if (is.null(directed)) 
    directed <- (nrow(symmetrise(net)) != nrow(net))
  library(igraph)
  net[,c("i","j")] <- net[,c("i","j")]-1
  if(directed) {
    g <- graph.edgelist(el=as.matrix(net[,c("i","j")]), directed=TRUE)
    g <- set.edge.attribute(g, "tnetw", value=net[,"w"])
  } else {
    g <- graph.edgelist(el=as.matrix(net[net[,"i"]<net[,"j"],c("i","j")]), directed=FALSE)
    g <- set.edge.attribute(g, "tnetw", value=net[net[,"i"]<net[,"j"],"w"])
  }
  if(gconly) {
    gc <- clusters(g, mode="strong")
    gc <- which(gc$membership==names(sort(-table(gc$membership)))[1])
    g <- subgraph(g, gc-1)
  }
  d <- shortest.paths(g, weights=get.edge.attribute(g, "tnetw"))
  diag(d) <- NA
  attributes(d)$nodes <- as.integer(V(g)+1)
  return(d)
}
