`rg_reshuffling_w` <-
function(net,option="weights",directed=NULL,seed=NULL){
  # Ensure that the network conforms to the tnet standard
  if (is.null(attributes(net)$tnet))                      net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet")   stop("Network not loaded properly")
  # Ensure that only one option is specified
  if(length(option)!=1)
     stop("you can only specify one option")
  # Check whether the edgelist is directed        
  if(is.null(directed))
    directed <- (nrow(symmetrise(net))!=nrow(net))
  # If seed is set, set it formally
  if(!is.null(seed))
    set.seed(as.integer(seed))

  ## Weight resuffling
  if(option=="weights") {
    if(!directed) 
      net <- net[net[,"i"]<net[,"j"],]
    net[,"w"] <- sample(net[,"w"])
    if(!directed) 
      net <- rbind(net, cbind(i=net[,"j"], j=net[,"i"], w=net[,"w"]))

  ## Weight and Link reshuffle
  } else if(option=="links") {
    library(igraph)
    if(!directed) 
      net <- net[net[,"i"]<net[,"j"],]
    net.i <- graph.edgelist(as.matrix(net[,1:2]), directed=directed)
    net.i <- rewire(net.i, niter = (ecount(net.i)*10))
    net.i <- get.edgelist(net.i)
    net[,1:2] <- net.i[order(net.i[,1], net.i[,2]),]
    if(directed) {
      net[,3] <- unlist(tapply(net[,3], cumsum(!duplicated(net[,1])), function(a) sample(a)))
    } else {
      net[,3] <- sample(net[,"w"]) 
      net <- rbind(net, cbind(i=net[,2], j=net[,1], w=net[,3]))
    }
  ## Local weight reshuffle
  } else if(option=="weights.local") {
    if(!directed)
      warning("The weights are not symmetric in the reshuffled network")
    klist <- degree_w(net)
    index <- net[,1] %in% klist[klist[,"degree"]>1,"node"]
    net[index,"w"] <- unlist(tapply(net[index,"w"], net[index,"i"], sample))

  ## A non-implemented option specified
  } else {
    stop("you must specify a correct option")
  }
  net <- net[order(net[,1], net[,2]),]
  net <- as.tnet(net, type = "weighted one-mode tnet")
  return(net)
}