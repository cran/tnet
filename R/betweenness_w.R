`betweenness_w` <-
function(edgelist, directed=NULL){
  edgelist     <- data.frame(i=as.integer(edgelist[,1]), j=as.integer(edgelist[,2]),w=edgelist[,3])
  dimnames(edgelist)[[2]] <- c("i","j","w")
  #no self-loops
  edgelist     <- edgelist[edgelist[,"i"]!=edgelist[,"j"],];
  #all positive weights
  edgelist     <- edgelist[edgelist[,"w"]>0,];
  #check if network is directed
  if(is.null(directed))
    directed   <- (nrow(symmetrise(edgelist))!=nrow(edgelist))
  if(!directed)
    edgelist <- edgelist[edgelist[,1]<edgelist[,2],]
  #Number of nodes and edges 
  N <- as.integer(max(c(edgelist[,1], edgelist[,2])))
  E <- as.integer(nrow(edgelist))
  #Elements for C-function
  EM <- as.integer(t(edgelist[,1:2])-1)
  EW <- as.numeric(1/edgelist[,3])
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
