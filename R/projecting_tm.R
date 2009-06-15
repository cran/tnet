`projecting_tm` <-
function(edgelist,method="sum"){
  if(ncol(edgelist)==2)
    edgelist <- cbind(edgelist, w=1)
  ## Support objects
  edgelist <- edgelist[order(edgelist[,"i"], edgelist[,"p"]),]
  np <- table(edgelist[,"p"])
  edgelist <- merge(edgelist, cbind(p=as.numeric(row.names(np)), np=np))
  net <- merge(edgelist, cbind(j=edgelist[,"i"],p=edgelist[,"p"]))
  net <- net[net[,"i"]!=net[,"j"],c("i","j","w","np")]
  net <- net[order(net[,"i"],net[,"j"]),]
  index <- !duplicated(net[,c("i","j")])  
  w <- switch(method,
      binary = rep(1, nrow(net)),
      sum    = tapply(net[,"w"], cumsum(index), sum),
      Newman = tapply(1:nrow(net), cumsum(index), function(a) sum(net[a,"w"]/(net[a,"np"]-1))))
  return(cbind(net[index,c("i","j")],w=w))
}