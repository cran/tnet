`rg_reshuffling_tm` <-
function(edgelist,seed=NULL){
  #Assign names to columns
  dimnames(edgelist)[[2]]<-c("i","p")
  #If seed is set, set it formally
  if(!is.null(seed))
    set.seed(as.integer(seed))
  rdm.2mode <- cbind(edgelist, ok=0)
  E.2mode <- nrow(rdm.2mode)
  while(sum(rdm.2mode[,"ok"])!= E.2mode) {
    rE <- which(rdm.2mode[,"ok"]==0)
    rdm.2mode[rE,2] <- rdm.2mode[sample(rE),2]
    rdm.2mode <- rdm.2mode[order(rdm.2mode[,1], rdm.2mode[,2]),]
    rdm.2mode[!duplicated(rdm.2mode[,1:2]),"ok"] <- 1
    if(sum(rdm.2mode[,"ok"])!= E.2mode)
      rdm.2mode[sample(1:E.2mode, size=(E.2mode-sum(rdm.2mode[,"ok"]))*3),"ok"] <- 0
  }
  rdm.2mode <- rdm.2mode[,1:2]
  dimnames(rdm.2mode)[[2]]<-c("i","p");
  rownames(rdm.2mode)<-NULL;
  return(rdm.2mode)
}