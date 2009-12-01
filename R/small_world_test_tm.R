`small_world_test_tm` <-
function(net, NR=1000, step=c(1,2)){
  cat("Running script written by Tore Opsahl\n")
  cat("(t.opsahl@imperial.ac.uk)\n\n")
  cat("Loading network\n")
  # Ensure that the network conforms to the tnet standard
  if(is.null(attributes(net)$tnet))                 net <- as.tnet(net, type="binary two-mode tnet")
  if(attributes(net)$tnet!="binary two-mode tnet")  stop("Network not loaded properly")
  # Basic network parameters
  Ni <- max(net[,1])
  cat(paste("-Number of 1st nodes:", Ni, "\n"))
  Np <- max(net[,2])
  cat(paste("-Number of 2nd nodes:", Np, "\n"))
  E <- nrow(net)
  cat(paste("-Number of ties:     ", E, "\n"))
  cat(paste("-Density:            ", E/(Ni*Np), "\n\n"))
  output <- list()
  indicator <- rep(as.logical(TRUE),NR)
  if(length(indicator)>50)
    indicator <- as.logical(c(rep(c(rep(FALSE, floor(length(indicator)/50)-1), 1),50), rep(FALSE, length(indicator)-length(rep(c(TRUE, rep(FALSE, floor(length(indicator)/50)-1)),50)))))
 
  cat("1: Calculating measures on observed network\n")
  if(1 %in% step) {
    cat("-Clustering coefficient\n")
    output[[1]] <- clustering_tm(net)
    cat(paste("--Binary:            ", output[[1]], "\n"))
    net.1 <- projecting_tm(net, method="binary")
    cat("-Distance ")
    output[[2]] <- distance_w(net.1)
    cat(paste("(based on the giant component of ", nrow(output[[2]]), ")\n", sep=""))
    cat(paste("--Binary:            ", mean(output[[2]], na.rm=TRUE), "\n\n"))
  } else {
    cat("    (skipped)\n")
  }
  cat("2: Calculating measures on random network\n")
  if(2 %in% step) {
    cat(paste("    (based on ", NR, " link reshuffled networks)\n", sep=""))
    output[[3]] <- matrix(data=0, nrow=NR, ncol=3)
    cat(paste("0%  10%  20%  30%  40%  50%  60%  70%  80%  90%  100%\n",
              "+----+----+----+----+----+----+----+----+----+----+\n|", sep=""));
    # Preform loop of all ties
    for (t in 1:NR) {
      if(indicator[t]) cat("|")
      net.r <- rg_reshuffling_tm(net, seed=t)
      output[[3]][t,1] <- clustering_tm(net.r)
      tmp <- distance_w(projecting_tm(net.r, method="binary"))
      output[[3]][t,2] <- mean(tmp, na.rm=TRUE)
      output[[3]][t,3] <- nrow(tmp)
    }
    cat("\n")
    randoml.output <- colMeans(output[[3]])
    cat("-Clustering coefficient\n")
    cat(paste("--Binary:            ", randoml.output[1], "\n"))
    cat(paste("-Distance (based on giant components of", randoml.output[3], "nodes on average)\n"))
    cat(paste("--Binary:            ", randoml.output[2], "\n"))
  } else {
    cat("    (skipped)\n")
  }
  return(output)
}

