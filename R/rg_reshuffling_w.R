`rg_reshuffling_w` <-
function(edgelist,option="weights",directed=NULL,seed=NULL){
  #Assign names to columns
  dimnames(edgelist)[[2]]<-c("i","j","w")
  #If seed is set, set it formally
  if(!is.null(seed))
    set.seed(as.integer(seed))
  if(length(option)!=1)
     stop("you can only specify one option")

  ## Weight resuffling
  if(option=="weights") {
    edgelist[,"w"] <- sample(edgelist[,"w"])

  ## Weight and Link reshuffle
  } else if(option=="links") {
    #Check whether the edgelist is directed        
    if(is.null(directed))
      directed <- (nrow(symmetrise(edgelist))!=nrow(edgelist))
    #If undirected, remove duplicated entires
    if(!directed) 
      edgelist <- edgelist[edgelist[,"i"]<edgelist[,"j"],]
    #Add an 'ok' column to the edgelist
    edgelist<-cbind(edgelist, ok=0)
    #Random loop until all ties are ok
    while(sum(edgelist[,"ok"])!=length(edgelist[,"ok"])) {
      #Randomise the creator of a tie
      edgelist[edgelist[,"ok"]==0,"i"] <-
        sample(edgelist[edgelist[,"ok"]==0,"i"])
      #Randomise the target of a tie
      edgelist[edgelist[,"ok"]==0,"j"] <-
        sample(edgelist[edgelist[,"ok"]==0,"j"])
      #If not self-loop: ok
      edgelist[edgelist[,"i"]!=edgelist[,"j"],"ok"]<-1;
      #If undirected, sort ids so that i < j
      if(directed==FALSE)
        edgelist[edgelist[,"i"]>edgelist[,"j"],c("i","j")] <- 
          edgelist[edgelist[,"i"]>edgelist[,"j"],c("j","i")];
      #Duplicates not ok
      edgelist[duplicated(edgelist[,c("i","j")]),"ok"] <- 0
      #If some ties are not ok, set some ok ones to be not ok
      if(sum(edgelist[,"ok"])!=length(edgelist[,"ok"])) {
        edgelist <- edgelist[order(-edgelist[,"ok"],
          sample(1:nrow(edgelist))),]
        edgelist[1:min(nrow(edgelist),
          sum(edgelist[,"ok"]==0)*2),"ok"] <- 0
      }
    }
    #Create duplicate entires for undirected ties
    if(directed==FALSE) 
      edgelist <- symmetrise(edgelist[,1:3]);
    #Order and return only 3 columns
    edgelist <- edgelist[order(edgelist[,"i"],edgelist[,"j"]),1:3];

  ## Local weight reshuffle
  } else if(option=="weights.local") {
    #Create an index for each node
    index <- !duplicated(edgelist[,"i"])
    #Local randomisation of the weights attached to a node's ties
    edgelist[,"w"] <- unlist(tapply(edgelist[,"w"], cumsum(index), 
                              function(a) sample(a, replace=FALSE)))
  # Something is wrong...
  } else {
    stop("you must specify a correct option")
  }
  dimnames(edgelist)[[2]]<-c("i","j","w");
  rownames(edgelist)<-NULL;
  return(edgelist)
}