`degree_w` <-
function(edgelist, measure=c("degree", "output"), 
self.loops=FALSE, type="out"){
  #Check that a valid measure is chosen
  if(length(which(measure=="degree" | measure=="output"))==0)
    stop("you must specify a valid option")
  edgelist <- as.data.frame(edgelist)
  #Reverse data if calculating in-degrees
  if(type == "in")
    edgelist <- data.frame(edgelist[,2], edgelist[,1], 
                           edgelist[,3])
  #Assign names to columns
  dimnames(edgelist)[[2]] <- c("i","j","w")
  #Remove self-loops?
  if(!self.loops)
    edgelist <- edgelist[edgelist[,"i"]!=edgelist[,"j"],];
  #Only positive ties
  edgelist <- edgelist[edgelist[,"w"]>0,];
  edgelist <- edgelist[order(edgelist[,"i"],edgelist[,"j"]),]
  #No duplication of ties in weighted edgelists
  edgelist <- edgelist[!duplicated(edgelist[,c("i","j")]),]

  ##Calculate measures
  #Create an index for each node
  index <- cumsum(!duplicated(edgelist[,1]))
  #Create output object
  k.list <- cbind(unique(edgelist[,1]), NaN, NaN)
  #Assign names
  dimnames(k.list)[[2]]<-c("vertex","degree","output")
  #Calculating degree?
  if(length(which(measure=="degree"))==1)
    k.list[,"degree"] <- tapply(edgelist[,"w"], index, length)
  #Calculating strength?
  if(length(which(measure=="output"))==1)
    k.list[,"output"] <- tapply(edgelist[,"w"], index, sum)
  #Add rows to the output object if isolates exists
  if(max(edgelist[,c("i","j")]) != nrow(k.list)) {
    k.list <- rbind(k.list, cbind(1:max(edgelist[,c("i","j")]),
      0, 0))
    k.list <- k.list[order(k.list[,"vertex"]),]
    k.list <- k.list[!duplicated(k.list[,"vertex"]),]
  }
  #Extract just relevant columns
  k.list <- k.list[,c("vertex", measure)]
  return(k.list)
}