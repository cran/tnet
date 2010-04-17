`betweenness_w` <-
function(net, directed = NULL, alpha=1){
  # Ensure that the network conforms to the tnet standard
  if (is.null(attributes(net)$tnet))                      net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet")   stop("Network not loaded properly")

  # Check if network is directed
  if(is.null(directed)) 
    directed <- (nrow(symmetrise(net)) != nrow(net))
  
  # Remove duplicated ties if undirected to conform with RBGL
  if(!directed) 
    net <- net[net[, 1] < net[, 2], ]

  # Number of nodes and edges 
  N <- as.integer(max(c(net[, 1], net[, 2])))
  E <- as.integer(nrow(net))

  # Elements for C-function
  EM <- as.integer(t(net[,1:2]) - 1)
  EW <- as.numeric(1/net[,3])^alpha

  # Run C function from RBGL
  library(RBGL)
  ans <- .Call("BGL_brandes_betweenness_centrality", N, E, EM, EW, PACKAGE = "RBGL")

  # Return output
  out <- list()
  out[[1]] <- cbind(node = 1:N, betweenness = as.numeric(ans[[1]]))
  out[[2]] <- cbind(t(matrix(EM, nrow = 2, ncol = E)) + 1, t(ans[[2]]))
  return(out)                       
}


