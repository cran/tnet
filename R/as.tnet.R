`as.tnet` <-
function(net, type=NULL){
  NC <- ncol(net)
  E <- nrow(net)
  if(is.null(type)) {
    type <- switch(as.character(NC),
      "2" = "binary two-mode tnet",
      "3" = "weighted one-mode tnet",
      "4" = "longitudinal tnet")
    cat(paste("Loaded data as", type, "\n"))
  }
  if(type == "binary two-mode tnet" & NC == 2) {
    dimnames(net)[[2]] <- c("i","p")
    net <- net[order(net[,"i"],net[,"p"]),]
    net <- net[!duplicated(net[,c("i","p")]),]
    if(nrow(net)!=E)
      stop("There are duplicated entries in the edgelist\n")
  } else if(type == "weighted two-mode tnet" & NC == 3) {
    dimnames(net)[[2]] <- c("i","p","w")
    net <- net[net[,"w"]>0,];
    if(nrow(net)!=E)
      stop("There are negative weights in the edgelist\n")
    net <- net[order(net[,"i"],net[,"p"]),]
    net <- net[!duplicated(net[,c("i","p")]),]
    if(nrow(net)!=E)
      stop("There are duplicated entries in the edgelist\n")
  } else if(type == "weighted one-mode tnet" & NC == 3) { 
    dimnames(net)[[2]] <- c("i","j","w")
    net <- net[net[,"i"]!=net[,"j"],];
    if(nrow(net)!=E) {
      cat("There were self-loops in the edgelist, these are removed\n")
      E <- nrow(net)  
    }
    net <- net[net[,"w"]>0,];
    if(nrow(net)!=E)
      stop("There are negative weights in the edgelist\n")
    net <- net[order(net[,"i"],net[,"j"]),]
    net <- net[!duplicated(net[,c("i","j")]),]
    if(nrow(net)!=E)
      stop("There are duplicated entries in the edgelist\n")
  } else if(type == "longitudinal tnet" & NC == 4) { 
    dimnames(net)[[2]] <- c("t","i","j","w")
    net <- net[net[,"w"]!=-1 | net[,"w"]!=1,];
    if(nrow(net)!=E)
      stop("There are weights that are not 1 or -1 in the edgelist\n")
    net <- net[order(net[,"i"],net[,"j"]),]
  } else {
    stop("Type of network not recognised\n")
  }
  rownames(net)<-NULL
  attributes(net)$tnet <- type
  return(net)
}