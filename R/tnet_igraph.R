`tnet_igraph` <- 
function(net, type=NULL, directed=NULL) {
  if (is.null(attributes(net)$tnet)) {
    if(is.null(type)) {
      net <- as.tnet(net)
    } else {
      net <- as.tnet(net, type=type)
    }
  }
  if (attributes(net)$tnet == "weighted one-mode tnet") {
    if(is.null(directed)) {
      tmp <- symmetrise_w(net, method = "MAX")
      directed <- (nrow(tmp) != nrow(net) | sum(tmp[,"w"]) != sum(net[,"w"]))
    }
    net[, c("i", "j")] <- net[, c("i", "j")] - 1
    if(directed) {
      g <- igraph0::graph.edgelist(el = as.matrix(net[, c("i", "j")]), directed = TRUE)
      g <- igraph0::set.edge.attribute(g, "weight", value = net[, "w"])
    } else {
      net <- net[net[,"i"]<net[,"j"],]
      g <- igraph0::graph.edgelist(el = as.matrix(net[,c("i","j")]), directed = FALSE)
      g <- igraph0::set.edge.attribute(g, "weight", value = net[, "w"])
    }
  } else {
    stop("igraph can currently only handle one-mode networks")
  }
  return(g)
}
