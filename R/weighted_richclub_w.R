`weighted_richclub_w` <-
function(net,rich="k", reshuffle="weights", samples=100, seed=NULL, directed=NULL){
  if (is.null(attributes(net)$tnet)) 
    net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet") 
    stop("Network not loaded properly")
  phi <- function(net, club, directed) {
    net <- net[order(net[,"i"], net[,"j"]), ]
    k.list <- degree_w(net, measure = club[2:length(club)])
    if(club[1] == "avg.w") 
      k.list <- cbind(k.list, avg.w = k.list[,"output"]/k.list[,"degree"])
    output <- unique(k.list[,club[1]])
    output <- cbind(x=output[order(output)], num=NaN, den=NaN, y=NaN)
    net <- cbind(net, rc.i=rep(k.list[,club[1]], k.list[,"degree"]))
    if(directed == FALSE) {
      net <- net[order(net[,"j"], net[,"i"]), ]
      net <- cbind(net, rc.j=rep(k.list[,club[1]], k.list[,"degree"]))
      net <- net[order(net[,"i"], net[,"j"]), ]
    } else {
      net <- cbind(net, rc.j=sapply(net[,"j"], function(a) sum(k.list[k.list[,"node"]==a, club[1]])))
    }
    net <- cbind(net, rc=pmin.int(net[,"rc.i"], net[,"rc.j"]))
    output[,"num"] <- sapply(output[,"x"], function(a) sum(net[net[,"rc"]>=a, "w"]))
    tmp.no.edges <- sapply(output[,"x"], function(a) length(net[net[,"rc"]>=a, "w"]))
    net <- net[order(-net[,"w"]), ]
    output[,"den"] <- sapply(output[,"x"], function(a) sum(net[1:tmp.no.edges[which(output[,"x"]==a)],"w"]))
    output[,"y"] <- output[,"num"]/output[,"den"]
    return(output)
  }
  if(!is.null(seed)) 
    set.seed(as.integer(seed))
  if(is.null(directed)) 
    directed <- (nrow(symmetrise(net)) != nrow(net))
  if(rich == "s" | rich == "w") {
    if(reshuffle == "weights.local" & !directed)
      cat("The observed network is undirected; however, the weights in randomisations are not symmetric\n")
    if(reshuffle != "weights.local")
      stop("The network cannot be reshuffled in that way as the nodes do not maintain their prominence")
  }
  club <- switch(rich,
     "k" = c("degree", "degree"),
     "s" = c("output", "degree", "output"),
     "w" = c("avg.w", "degree", "output"))
  observed.phi <- phi(net=net, club=club, directed=directed)
  observed <- matrix(data=NaN, nrow=max(observed.phi[, "x"])+1, ncol=3)
  row.names(observed) <- 0:max(observed.phi[, "x"])
  dimnames(observed)[[2]] <- c("obs", "rdm", "rate")
  for (j in observed.phi[, "x"]) observed[j + 1, "obs"] <- observed.phi[which(observed.phi[,"x"] == j), "y"]
  random.m <- matrix(data = NaN, nrow = samples, ncol = nrow(observed))
  for (i in 1:samples) {
    invisible(capture.output(rnet <- rg_reshuffling_w(net = net, option = reshuffle, directed = directed)))
    random.phi <- phi(net = rnet, club = club, directed = directed)
    for (j in random.phi[, "x"]) 
      random.m[i, j + 1] <- random.phi[which(random.phi[,"x"] == j), "y"]
    if(i/100 == round(i/100)) 
      cat(paste("Random network ",i,"/",samples," @ ", date(),"\n",sep = ""))
  }
  observed[,"rdm"] <- colMeans(random.m)
  observed[,"rate"] <- observed[,"obs"]/observed[,"rdm"]
  observed <- observed[!is.na(observed[,"obs"]), ]
  observed <- observed[!is.na(observed[,"rdm"]), ]
  observed <- observed[!is.na(observed[,"rate"]), ]
  return(observed)
}