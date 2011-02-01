`longitudinal_data_to_edgelist` <-
function(ld){
  #name columns
  dimnames(ld)[[2]]<-c("t","i","j","w")
  #remove time column
  ld <- ld[,c("i","j","w")]
  #remove joining and leaving of nodes
  ld <- ld[ld[,"i"]!=ld[,"j"],]
  #order by creator and target node
  ld <- ld[order(ld[,"i"], ld[,"j"]),]
  #edge index
  index <- !duplicated(ld[,1:2])
  #create edgelist
  net <- data.frame(ld[index,c("i","j")], w=0)
  dimnames(net)[[2]]<-c("i","j","w")
  #find weights of ties
  net[,"w"] <- tapply(ld[,"w"], cumsum(index), sum)
  row.names(net)<-NULL
  return(net)
}