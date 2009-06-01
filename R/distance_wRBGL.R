`distance_w` <-
function(edgelist,directed=NULL,gconly=TRUE){
  edgelist     <- as.matrix(edgelist)
  dimnames(edgelist)[[2]] <- c("i","j","w")
  #no self-loops
  edgelist     <- edgelist[edgelist[,"i"]!=edgelist[,"j"],];
  #all positive weights
  edgelist     <- edgelist[edgelist[,"w"]>0,];               
  #inversion
  edgelist[,3] <- 1/edgelist[,"w"]     
  #check whether the edgelist is directed        
  if(is.null(directed))
    directed <- (nrow(symmetrise(edgelist))!=nrow(edgelist))
  #nodes
  n            <- max(edgelist[,c("i","j")])                 
  #distance matrix
  d            <- matrix(data=Inf, ncol=n, nrow=n)   
  #convert the data so that the RBGL package can read it.        
  library(RBGL);
  V <- as.character(1:n);
  E <- vector("list", length=n);
  names(E) <- V;
  for(i in 1:n)
    E[[i]] <- list(edges=edgelist[edgelist[,"i"]==i,"j"], 
                weights=edgelist[edgelist[,"i"]==i,"w"])
  if(directed) {
    g <- new("graphNEL", nodes=V, edgeL=E, edgemode="directed")
  } else {
    g <- new("graphNEL", nodes=V, edgeL=E, edgemode="undirected")
  }
  #Extract giant component
  gc <- nodes(g)
  if(gconly) {
    gc <- connComp(g)
    gc <- gc[[which.max(sapply(gc,length))]]
    g <- subGraph(gc, g)
  }
  #Calculate the distances
  d <- t(sapply(gc, function(a) 
          dijkstra.sp(g, start=a)$distances))
  diag(d) <- NA
  return(d)
}