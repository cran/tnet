`clustering_w` <-
function(edgelist,measure="mi"){
  edgelist  <- as.matrix(edgelist)
  dimnames(edgelist)[[2]] <- c("i","j","wij")
  #no self-loops
  edgelist <- edgelist[edgelist[,"i"]!=edgelist[,"j"],];
  #all positive weights
  edgelist <- edgelist[edgelist[,"wij"]>0,];
  #find nodes at step 2, k nodes
  ks       <- sapply(edgelist[,"j"], 
                function(a) edgelist[edgelist[,"i"]==a,"j"]); 
  #find the weight of ties from step 1 to step 2, w_jk
  wjk      <- sapply(edgelist[,"j"], 
                function(a) edgelist[edgelist[,"i"]==a,"wij"]); 
  #create a list of triplets
  triplets <- cbind(edgelist[rep(1:nrow(edgelist), 
                    lapply(ks, length)),], k=unlist(ks), 
                    wjk=unlist(wjk))
  rm(ks,wjk)
  #find closed triplets
  dimnames(edgelist)[[2]] <- c("i","k","wik")
  triplets <- merge(triplets,edgelist, all.x=TRUE)
  #remove 1-step loops, i->j->i
  triplets <- triplets[triplets[,"i"] != triplets[,"k"],
                      c("i","j","k","wij","wjk","wik")]
  triplets[is.na(triplets[,"wik"]),"wik"] <- 0
  #calculate triplet values
  triplets <- cbind(triplets,tam=NaN,tgm=NaN,tmi=NaN,tma=NaN)
  triplets[,"tam"]<- 0.5*(triplets[,"wij"]+triplets[,"wjk"])
  triplets[,"tgm"]<- sqrt(triplets[,"wij"]*triplets[,"wjk"])
  triplets[,"tmi"]<- pmin.int(triplets[,"wij"],triplets[,"wjk"])
  triplets[,"tma"]<- pmax.int(triplets[,"wij"],triplets[,"wjk"])
  #output results
  output <- rep(0, length(measure))
  j = 1
  for(i in measure) {
    output[j] <- switch(i,
      "am" = sum(triplets[triplets[,"wik"]>0,"tam"])/
             sum(triplets[,"tam"]),
      "gm" = sum(triplets[triplets[,"wik"]>0,"tgm"])/
             sum(triplets[,"tgm"]),
      "mi" = sum(triplets[triplets[,"wik"]>0,"tmi"])/
             sum(triplets[,"tmi"]),
      "ma" = sum(triplets[triplets[,"wik"]>0,"tma"])/
             sum(triplets[,"tma"]))
    j <- j+1
  }
  return(output)
}