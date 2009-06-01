`clustering_w_barrat` <-
function(edgelist, measure="am"){
  #Assign names to columns
  dimnames(edgelist)[[2]] <- c("i","j","w")
  #Find basic parameters
  N <- max(c(edgelist[,"i"],edgelist[,"j"]))
  E <- nrow(edgelist)
  #Ensure network is undirected
  tmp <- rbind(edgelist, cbind(i=edgelist[,"j"], j=edgelist[,"i"], w=0))
  tmp <- tmp[!duplicated(tmp[,c("i","j")]),]
  if(nrow(tmp) != E) 
    stop("Network is not undirected!\nMeasure is not defined from directed networks.\n")
  #Create output object
  edgelist <- edgelist[order(edgelist[,"i"], edgelist[,"j"]),]
  index <- edgelist[,"i"]
  output <- cbind(vertex=1:N, degree=0, strength=0, am=NaN, gm=NaN, ma=NaN, mi=NaN)
  output[unique(index), "degree"] <- tapply(edgelist[,"w"], index, length)
  output[unique(index), "strength"] <- tapply(edgelist[,"w"], index, sum)
  #Define numerator-support table
  tri <- cbind(edgelist[,c("i","j")], 1)
  dimnames(tri)[[2]] <- c("j","h","closed")
  #For every node
  for(i in output[output[,"degree"]>=2,"vertex"]) {
    js <- hs <- edgelist[edgelist[,"i"]==i,c("j","w")]
    dimnames(js)[[2]] <- c("j","wij")
    dimnames(hs)[[2]] <- c("h","wih")
    #All possible ties
    jhs <- merge(js, hs)
    jhs <- jhs[jhs[,"j"]!=jhs[,"h"],]
    jhs <- jhs[,c("j","h","wij","wih")]
    #Find closing ties
    jhs <- merge(jhs, tri, all.x=TRUE)
    jhs[is.na(jhs[,"closed"]),"closed"] <- 0
    jhs <- jhs[,c("wij","wih","closed")]
    jhs <- cbind(jhs, AM=(jhs[,1]+jhs[,2])*0.5,
                      GM=sqrt(jhs[,1]*jhs[,2]),
                      MA=pmax(jhs[,1],jhs[,2]),
                      MI=pmin(jhs[,1],jhs[,2]))
    #Calculate ratios
    output[i,"am"] <- sum(jhs[jhs[,"closed"]==1,"AM"])/sum(jhs[,"AM"])
    output[i,"gm"] <- sum(jhs[jhs[,"closed"]==1,"GM"])/sum(jhs[,"GM"])
    output[i,"ma"] <- sum(jhs[jhs[,"closed"]==1,"MA"])/sum(jhs[,"MA"])
    output[i,"mi"] <- sum(jhs[jhs[,"closed"]==1,"MI"])/sum(jhs[,"MI"])
  }
  #Return output
  return(output[,c("vertex",measure)])
}



