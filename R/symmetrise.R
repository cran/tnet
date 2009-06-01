`symmetrise` <-
function(edgelist,method="MAX"){
  edgelist <- as.matrix(edgelist)
  #Assign names to columns
  dimnames(edgelist)[[2]] <- c("i","j","w")
  #Join the edgelist with it's reversed version
  edgelist <- rbind(edgelist, cbind(i=edgelist[,"j"], 
                j=edgelist[,"i"], w=0))
  #Remove exact duplicates (i=i & j=j)
  edgelist <- edgelist[!duplicated(edgelist[,c("i","j")]),]
  #Change ties so that i<j
  edgelist[edgelist[,"i"]>edgelist[,"j"],c("i","j")] <- 
    edgelist[edgelist[,"i"]>edgelist[,"j"],c("j","i")];
  #Order ties (the greatest weight first)
  edgelist <- 
    edgelist[order(edgelist[,"i"],edgelist[,"j"], -edgelist[,"w"]),]
  #Create an index
  dup <- cumsum(rep.int(c(1,0), nrow(edgelist)/2))
  #Create a weight vector
  w <- switch(method,
    MAX   = edgelist[rep(c(TRUE,FALSE), length=nrow(edgelist)),"w"],
    MIN   = edgelist[rep(c(FALSE,TRUE), length=nrow(edgelist)),"w"],
    AMEAN = tapply(edgelist[,"w"], dup, mean),
    SUM   = tapply(edgelist[,"w"], dup, sum),
    GMEAN = tapply(edgelist[,"w"], dup, function(a) sqrt(a[1]*a[2])),
    PROD  = tapply(edgelist[,"w"], dup, function(a) a[1]*a[2]),
    DIFF  = tapply(edgelist[,"w"], dup, function(a) abs(a[1]-a[2])))
  #Extract only one entry per undirected tie
  edgelist <- edgelist[rep(c(TRUE,FALSE), length=nrow(edgelist)),]
  #Add the weight vector to this list
  edgelist[,"w"] <- w;
  #Only keep ties with a positive weight
  edgelist <- edgelist[edgelist[,3]>0,]
  #Join this edgelist with its reversed version
  edgelist <- rbind(cbind(edgelist[,1],edgelist[,2],edgelist[,3]),
                cbind(edgelist[,2],edgelist[,1],edgelist[,3]))
  #Assign names to columns
  dimnames(edgelist)[[2]]<-c("i","j","w")
  #Order edgelist
  edgelist <- edgelist[order(edgelist[,"i"],edgelist[,"j"]),]
  row.names(edgelist)<-NULL
  return(edgelist)
}
