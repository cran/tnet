`clustering_w` <-
function(net,measure="mi",subsample=1,seed=NULL){
  # Ensure that the network conforms to the tnet standard
  if (is.null(attributes(net)$tnet))                      net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet")   stop("Network not loaded properly")
  if(!is.null(seed))
    set.seed(as.integer(seed))

  # List of edges split by sender
  net.list.j <- split(net[,"j"], net[,"i"])
  net.list.wij <- split(net[,"w"], net[,"i"])
  # Find nodes at step 2: k nodes
  net.list.k <- sapply(net[,"j"], function(a) net.list.j[[as.character(a)]]); 
  rm(net.list.j)
  # Find the weight of ties from step 1 to step 2: w_jk
  net.list.wjk <- sapply(net[,"j"], function(a) net.list.wij[[as.character(a)]]); 
  rm(net.list.wij)
  # Create a list of triplets
  ks <- unlist(lapply(net.list.k, length))
  if(subsample!=1) {
    if(subsample<1) {
      index <- sample.int(sum(ks), round(sum(ks)*subsample))
    } else {
      index <- sample.int(sum(ks), as.integer(subsample))
    }
    index <- index[order(index)]
    net.list.i <- net[rep(1:nrow(net), ks),"i"][index]
    net.list.wij <- net[rep(1:nrow(net), ks),"w"][index]
    net.list.k <- unlist(net.list.k)[index]
    net.list.wjk <- unlist(net.list.wjk)[index]
  } else {
    net.list.i <- net[rep(1:nrow(net), ks),"i"]
    net.list.wij <- net[rep(1:nrow(net), ks),"w"]
    net.list.k <- unlist(net.list.k)
    net.list.wjk <- unlist(net.list.wjk)
    
  }
  triplets <- data.frame(i=net.list.i, k=net.list.k, wij=net.list.wij, wjk=net.list.wjk)
  rm(ks, net.list.i, net.list.k, net.list.wij, net.list.wjk)
  # Remove 1-step loops, i->j->i
  triplets <- triplets[triplets[,"i"] != triplets[,"k"],]
  # Find closed triplets
  net <- data.frame(i=net[,"i"], k=net[,"j"], wik=TRUE)
  triplets <- merge(triplets, net, all.x=TRUE)
  triplets[is.na(triplets[,"wik"]),"wik"] <- FALSE
  # Calculate triplet values
  triplets <- cbind(triplets,tam=NaN,tgm=NaN,tmi=NaN,tma=NaN)
  triplets[,"tam"]<- 0.5*(triplets[,"wij"]+triplets[,"wjk"])
  triplets[,"tgm"]<- sqrt(triplets[,"wij"]*triplets[,"wjk"])
  triplets[,"tmi"]<- pmin.int(triplets[,"wij"],triplets[,"wjk"])
  triplets[,"tma"]<- pmax.int(triplets[,"wij"],triplets[,"wjk"])
  # Output results
  output <- rep(0, length(measure))
  j <- 1
  for(i in measure) {
    output[j] <- switch(i,
      "am" = sum(triplets[triplets[,"wik"],"tam"])/sum(triplets[,"tam"]),
      "gm" = sum(triplets[triplets[,"wik"],"tgm"])/sum(triplets[,"tgm"]),
      "mi" = sum(triplets[triplets[,"wik"],"tmi"])/sum(triplets[,"tmi"]),
      "ma" = sum(triplets[triplets[,"wik"],"tma"])/sum(triplets[,"tma"]),
      "bi" = nrow(triplets[triplets[,"wik"],])/nrow(triplets))
    j <- j+1
  }
  names(output) <- measure
  return(output)
}
