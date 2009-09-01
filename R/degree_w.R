`degree_w` <-
function(net, measure=c("degree", "output"), type="out"){
  if(is.null(attributes(net)$tnet))
    net <- as.tnet(net, type="weighted one-mode tnet")
  if(attributes(net)$tnet!="weighted one-mode tnet")
    stop("Network not loaded properly")
  #Reverse data if calculating in-degrees
  if(type == "in") {
    net <- data.frame(i=net[,2], j=net[,1], w=net[,3])
    net <- net[order(net[,"i"],net[,"j"]),]
  }
  ##Calculate measures
  #Create an index for each node
  index <- cumsum(!duplicated(net[,1]))
  #Create output object
  k.list <- cbind(unique(net[,1]), NaN, NaN)
  #Assign names
  dimnames(k.list)[[2]]<-c("node","degree","output")
  #Calculating degree?
  if(length(which(measure=="degree"))==1)
    k.list[,"degree"] <- tapply(net[,"w"], index, length)
  #Calculating strength?
  if(length(which(measure=="output"))==1)
    k.list[,"output"] <- tapply(net[,"w"], index, sum)
  #Add rows to the output object if isolates exists
  if(max(net[,c("i","j")]) != nrow(k.list)) {
    k.list <- rbind(k.list, cbind(1:max(net[,c("i","j")]),
      0, 0))
    k.list <- k.list[order(k.list[,"node"]),]
    k.list <- k.list[!duplicated(k.list[,"node"]),]
  }
  #Extract just relevant columns
  k.list <- k.list[,c("node", measure)]
  return(k.list)
}