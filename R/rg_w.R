`rg_w` <-
function(nodes=100,arcs=300,max.weight=10,directed=TRUE,seed=NULL){
  # If seed is set, set if formally
  if (!is.null(seed))
    set.seed(as.integer(seed))
  if(arcs > 1) {
    # Create random edgelist object with 50% more ties than needed
    rg_w <- data.frame(i=sample(1:nodes, (arcs*1.5), replace=TRUE), 
                       j=sample(1:nodes, (arcs*1.5), replace=TRUE),
                       w=sample(1:max.weight, (arcs*1.5), replace=TRUE))
    # Remove self-loops
    rg_w <- rg_w[rg_w[,1]!=rg_w[,2],]
    # Remove duplicated entries, and extract the right number of ties
    if(directed) {
      rg_w <- rg_w[!duplicated(rg_w[,1:2]),]
      rg_w <- rg_w[sample(1:nrow(rg_w), arcs),]
    } else {
      rg_w <- rg_w[rg_w[,1]<rg_w[,2],]
      rg_w <- rg_w[!duplicated(rg_w[,1:2]),]
      rg_w <- rg_w[sample(1:nrow(rg_w), arcs*0.5),]
      rg_w <- rbind(as.matrix(rg_w), cbind(rg_w[,2], rg_w[,1], rg_w[,3]))
    }
  } else {
    # Create a random edgelist object based on probabilities
    rg_w <- which(matrix(data=runif(nodes^2), nrow=nodes, ncol=nodes)<arcs, arr.ind=TRUE)
    rg_w <- rg_w[rg_w[,1]!=rg_w[,2],]
    rg_w <- cbind(rg_w, sample(1:max.weight, nrow(rg_w), replace=TRUE))
    if(!directed) {
      rg_w <- rg_w[rg_w[,1]<rg_w[,2],]
      rg_w <- rbind(rg_w, cbind(rg_w[,2], rg_w[,1], rg_w[,3]))
    }
  }
  return(as.tnet(rg_w, type="weighted one-mode tnet"))
}