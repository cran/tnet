`dichotomise` <-
function(net,GT=0){
  if(is.null(attributes(net)$tnet))
    net <- as.tnet(net, type="weighted one-mode tnet")
  if(attributes(net)$tnet!="weighted one-mode tnet")
    stop("Network not loaded properly")
  #Extract ties with a weight greater than GT
  net <- net[net[,3]>GT,]
  #Set their weight to 1
  net[,3] <- 1
  row.names(net)<-NULL
  #Assign names to net
  dimnames(net)[[2]]<-c("i","j","w")
  return(net)
}