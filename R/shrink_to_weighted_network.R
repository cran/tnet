`shrink_to_weighted_network` <-
function(net){
  net <- as.data.frame(net)
  #Order net
  net <- net[order(net[,1],net[,2]),]
  #Find duplicates
  net[,3] <- as.numeric(!duplicated(net[,1:2]))
  #Create an index of ties
  net[,4] <- cumsum(net[,3])
  #Count duplications
  net[net[,3]==1,5] <- tapply(net[,3], 
    net[,4], length)
  #Extract relevant columns
  net <- net[net[,3]==1,c(1,2,5)]
  return(as.tnet(net, type="weighted one-mode tnet"))
}