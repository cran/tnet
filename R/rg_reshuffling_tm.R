`rg_reshuffling_tm` <-
function(net,option="links",seed=NULL){
  if(option!="links")
    stop("Only link reshuffling is currently implemented")
  # Ensure that the network conforms to the tnet standard
  if(is.null(attributes(net)$tnet))                 net <- as.tnet(net, type="binary two-mode tnet")
  if(attributes(net)$tnet!="binary two-mode tnet")  stop("Network not loaded properly")
  # If seed is set, set it formally
  if(!is.null(seed))
    set.seed(as.integer(seed))
  rnet <- cbind(net, ok=0)
  E <- nrow(rnet)
  while(sum(rnet[,"ok"])!= E) {
    rE <- which(rnet[,"ok"]==0)
    rnet[rE,2] <- rnet[sample(rE),2]
    rnet <- rnet[order(rnet[,1], rnet[,2]),]
    rnet[,"ok"] <- as.integer(!duplicated(rnet[,1:2]))
    if(sum(rnet[,"ok"])!= E)
      rnet[sample(1:E, size=min(c((E-sum(rnet[,"ok"]))*10, E))),"ok"] <- 0
  }
  rnet <- rnet[,1:2]
  dimnames(rnet)[[2]]<-c("i","p");
  rownames(rnet)<-NULL;
  attributes(rnet)$tnet <- "binary two-mode tnet"
  return(rnet)
}
