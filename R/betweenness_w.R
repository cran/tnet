`betweenness_w` <-
function(net, directed=NULL){
  net     <- data.frame(i=as.integer(net[,1]), j=as.integer(net[,2]),w=net[,3])
  dimnames(net)[[2]] <- c("i","j","w")
  #no self-loops
  net     <- net[net[,"i"]!=net[,"j"],];
  #all positive weights
  net     <- net[net[,"w"]>0,];
  #check if network is directed
  if(is.null(directed))
    directed   <- (nrow(symmetrise(net))!=nrow(net))
  if(!directed)
    net <- net[net[,1]<net[,2],]
  #Number of nodes and edges 
  N <- as.integer(max(c(net[,1], net[,2])))
  E <- as.integer(nrow(net))
  #Elements for C-function
  EM <- as.integer(t(net[,1:2])-1)
  EW <- as.numeric(1/net[,3])
  #Possible fix for 1/3, but not working when 2x1/6 and 1x1/3
  ##EW <- as.numeric(ceiling(1000000*EW))
  #Run C function from RBGL
  library(RBGL);
  ans <- .Call("BGL_brandes_betweenness_centrality", N, E, EM, EW, PACKAGE = "RBGL")
  #Return output
  out <- list()
  out[[1]] <- cbind(vertex=1:N, betweenness=as.numeric(ans[[1]]))
  out[[2]] <- cbind(t(matrix(EM, nrow=2, ncol=E))+1, t(ans[[2]]))
  return(out)                         
}
