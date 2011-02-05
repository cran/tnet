`as.tnet` <-
function(net, type=NULL){
  NC <- ncol(net)
  E <- nrow(net)
  if(is.null(type)) {
    type <- switch(as.character(NC),
      "2" = "binary two-mode tnet",
      "3" = "weighted one-mode tnet",
      "4" = "longitudinal tnet")
    if(is.null(type) & NC == E)
      type <- "weighted one-mode tnet"
    if(is.null(type) & sum(net==1 | net==0)==NC*E)
      type <- "binary two-mode tnet"
    if(is.null(type) & sum(net!=1)>0)
      type <- "weighted two-mode tnet"
    if(is.null(type))
      stop("Could not determine the type of network. Specify the type-parameter.")
    warning(paste("Data assumed to be", type, "(if this is not correct, specify type)"))
  }
  if(NC>4) {
    if(type == "binary two-mode tnet") {
      net <- which(net>0, arr.ind=TRUE)
    } else if(type == "weighted two-mode tnet" | type == "weighted one-mode tnet") {
      net <- cbind(which(net>0, arr.ind=TRUE), net[net>0])
    } else {
      stop("Issues converting matrix format to edgelist. Check manual for datatypes.")
    }
    NC <- ncol(net)
    E <- nrow(net)
  }
  if(type == "binary two-mode tnet" & NC == 2) {
    dimnames(net)[[2]] <- c("i","p")
    net <- net[order(net[,"i"],net[,"p"]),]
    net <- net[!duplicated(net[,c("i","p")]),]
    if(nrow(net)!=E)
      stop("There are duplicated entries in the edgelist")
    if(sum(is.na(suppressWarnings(as.integer(c(net[,"i"],net[,"p"])))))>0 | sum(as.integer(c(net[,"i"],net[,"p"]))==c(net[,"i"],net[,"p"]))!=2*E)
      stop("Not all node id's are integers")
    if(min(c(net[,"i"],net[,"p"]))<1)
      stop("A node id's below 1 is detected. The lowest possible node id is 1.")

  } else if(type == "weighted two-mode tnet" & NC == 3) {
    dimnames(net)[[2]] <- c("i","p","w")
    net <- net[net[,"w"]>0,];
    if(nrow(net)!=E)
      stop("There are negative weights in the edgelist")
    net <- net[order(net[,"i"],net[,"p"]),]
    net <- net[!duplicated(net[,c("i","p")]),]
    if(nrow(net)!=E)
      stop("There are duplicated entries in the edgelist")
    if(sum(is.na(suppressWarnings(as.integer(c(net[,"i"],net[,"p"])))))>0 | sum(as.integer(c(net[,"i"],net[,"p"]))==c(net[,"i"],net[,"p"]))!=2*E)
      stop("Not all node id's are integers")
    if(min(c(net[,"i"],net[,"p"]))<1)
      stop("A node id's below 1 is detected. The lowest possible node id is 1.")

  } else if(type == "weighted one-mode tnet" & NC == 3) { 
    dimnames(net)[[2]] <- c("i","j","w")
    net <- net[net[,"i"]!=net[,"j"],];
    if(nrow(net)!=E) {
      warning("There were self-loops in the edgelist, these were removed")
      E <- nrow(net)  
    }
    net <- net[net[,"w"]>0,];
    if(nrow(net)!=E)
      stop("There are negative weights in the edgelist")
    net <- net[order(net[,"i"],net[,"j"]),]
    net <- net[!duplicated(net[,c("i","j")]),]
    if(nrow(net)!=E)
      stop("There are duplicated entries in the edgelist")
    if(sum(net[,1]<net[,2])==E | sum(net[,1]>net[,2])==E)
      warning("The network might be undirected. If this is the case, each tie should be mention twice. The symmetrise-function can be used to include reverse version of each tie.")
    if(sum(is.na(suppressWarnings(as.integer(c(net[,"i"],net[,"j"])))))>0 | sum(as.integer(c(net[,"i"],net[,"j"]))==c(net[,"i"],net[,"j"]))!=2*E)
      stop("Not all node id's are integers")
    if(min(c(net[,"i"],net[,"j"]))<1)
      stop("A node id's below 1 is detected. The lowest possible node id is 1.")

  } else if(type == "longitudinal tnet" & NC == 4) { 
    dimnames(net)[[2]] <- c("t","i","j","w")

    net <- net[net[,"w"]!=-1 | net[,"w"]!=1,];
    if(nrow(net)!=E)
      stop("There are weights that are not 1 or -1 in the edgelist")
    net <- net[order(net[,"i"],net[,"j"]),]
    if(sum(is.na(suppressWarnings(as.integer(c(net[,"i"],net[,"j"])))))>0 | sum(as.integer(c(net[,"i"],net[,"j"]))==c(net[,"i"],net[,"j"]))!=2*E)
      stop("Not all node id's are integers")
    if(min(c(net[,"i"],net[,"j"]))<1)
      stop("A node id's below 1 is detected. The lowest possible node id is 1.")
    N <- length(unique(c(net[,"i"],net[,"j"])))
    if(sum(net[,"i"]==net[,"j"] & net[,"w"]==1) == 0) {
      warning("Adding node joining data")
      tmp <- rbind(data.frame(t=net[,"t"], n=net[,"i"]), data.frame(t=net[,"t"], n=net[,"j"])) 
      tmp <- tmp[order(tmp[,"t"]),]
      tmp <- tmp[!duplicated(tmp[,"n"]),]
      tmp <- data.frame(t=tmp[,"t"], i=tmp[,"n"], j=tmp[,"n"], w=1)
      net <- rbind(tmp, net)
      net <- net[order(net[,"t"]),]
    }
    if(sum(net[,"i"]==net[,"j"] & net[,"w"]==1) != N)
      stop("Problem with node joining data")
    if(class(net[,"t"])[1]!="POSIXct") {
      if(class(net[,"t"])!="character") {
        net[,"t"] <- as.character(net[,"t"])
        net[,"t"] <- as.POSIXct(net[,"t"])
      }
    }
  } else {
    stop("Type of network not recognised\n")
  }
  rownames(net)<-NULL
  attributes(net)$tnet <- type
  return(net)
}