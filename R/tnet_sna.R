`tnet_sna` <- 
function(net, type=NULL) {
  if (is.null(attributes(net)$tnet)) {
    if(is.null(type)) {
      net <- as.tnet(net)
    } else {
      net <- as.tnet(net, type=type)
    }
  }
  if (attributes(net)$tnet == "weighted one-mode tnet") {
    N <- max(c(net[,"i"], net[,"j"]))
    g <- matrix(data=0, nrow=N, ncol=N)
    warning("Tie weights are lost")
    g[as.matrix(net[,c("i","j")])] <- 1
    g <- sna::as.sociomatrix.sna(g)

  } else if (attributes(net)$tnet == "binary two-mode tnet") {
    N1 <- max(net[,"i"])
    N2 <- max(net[,"p"])
    g <- matrix(data=0, nrow=N1, ncol=N2)
    g[as.matrix(net[,c("i","p")])] <- 1
    g <- sna::as.sociomatrix.sna(g, force.bipartite = TRUE)

  } else if (attributes(net)$tnet == "weighted two-mode tnet") {
    warning("Tie weights are lost")
    N1 <- max(net[,"i"])
    N2 <- max(net[,"p"])
    g <- matrix(data=0, nrow=N1, ncol=N2)
    g[as.matrix(net[,c("i","p")])] <- 1
    g <- sna::as.sociomatrix.sna(g, force.bipartite = TRUE)

  } else {
    stop("sna can currently only handle one-mode and two-mode networks")
  }
  return(g)
}
