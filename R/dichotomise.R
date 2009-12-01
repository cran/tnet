`dichotomise` <-
function(net,GT=0){
  # Ensure that the network conforms to the tnet standard
  if (is.null(attributes(net)$tnet))                      net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet")   stop("Network not loaded properly")

  # Extract ties with a weight greater than GT
  net <- net[net[,"w"]>GT,]
  if(nrow(net)==0)
    warning("There were no ties with a weight greater than the cutoff")
  # Set their weight to 1
  net[,"w"] <- 1
  row.names(net)<-NULL
  return(net)
}