`rg_w` <-
function(nodes=100,arcs=300,max.weight=10,directed=TRUE,seed=NULL){
  #If seed is set, set if formally
  if (!is.null(seed))
    set.seed(as.integer(seed))
  #Create random edgelist object with 33% more ties than needed
  rg_w <- data.frame(i=sample(1:nodes, (arcs*1.33), replace=TRUE), 
                   j=sample(1:nodes, (arcs*1.33), replace=TRUE),
                   w=sample(1:max.weight, (arcs*1.33), replace=TRUE))
  #Remove self-loops
  rg_w <- rg_w[rg_w[,1]!=rg_w[,2],]
  #Remove duplicated entries
  #Extract the right number of ties
  if(directed==TRUE) {
    rg_w <- rg_w[!duplicated(rg_w[,1:2]),]
    rg_w <- rg_w[1:arcs,]
  } else {
    rg_w[rg_w[,1]>rg_w[,2],1:2] <- rg_w[rg_w[,1]>rg_w[,2],2:1];
    rg_w <- rg_w[!duplicated(rg_w[,1:2]),]
    rg_w <- rg_w[1:as.integer(arcs*0.5),]
    #Symmetrise
    rg_w <- rbind(cbind(rg_w[,1],rg_w[,2],rg_w[,3]), 
                  cbind(rg_w[,2],rg_w[,1],rg_w[,3]))
  }
  rg_w <- rg_w[order(rg_w[,1], rg_w[,2]),]
  dimnames(rg_w)[[2]]<-c("i","j","w")
  rownames(rg_w)<-NULL
  return(rg_w)
}