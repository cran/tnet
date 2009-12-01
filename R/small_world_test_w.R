`small_world_test_w` <-
function(net, NR=1000, step=c(1,2,3)){
  # Ensure that the network conforms to the tnet standard
  if (is.null(attributes(net)$tnet))                      net <- as.tnet(net, type = "weighted one-mode tnet")
  if (attributes(net)$tnet != "weighted one-mode tnet")   stop("Network not loaded properly")

  cat("Running script written by Tore Opsahl\n")
  cat("(t.opsahl@imperial.ac.uk)\n\n")
  cat("Loading network\n")
  # Basic network parameters
  N <- max(c(net[,1], net[,2]))
  cat(paste("-Number of nodes:    ", N, "\n"))
  E <- nrow(net)
  cat(paste("-Number of ties:     ", E, "\n"))
  cat(paste("-Density:            ", E/(N*(N-1)), "\n\n"))
  net.b <- dichotomise(net)
  directed <- (nrow(symmetrise(net.b))!=nrow(net.b))
  output <- list()
  indicator <- rep(as.logical(TRUE),NR)
  if(length(indicator)>50)
    indicator <- as.logical(c(rep(c(rep(FALSE, floor(length(indicator)/50)-1), 1),50), rep(FALSE, length(indicator)-length(rep(c(TRUE, rep(FALSE, floor(length(indicator)/50)-1)),50)))))
 
  cat("1: Calculating measures on observed network\n")
  if(1 %in% step) {
    cat("-Clustering coefficient\n")
    output[[1]] <- clustering_w(net, measure=c("am", "gm", "ma", "mi","bi"))
    cat(paste("--Binary:            ", output[[1]][5], "\n"))
    cat(paste("--Weighted (a mean): ", output[[1]][1], "\n"))
    cat(paste("--Weighted (g mean): ", output[[1]][2], "\n"))
    cat(paste("--Weighted (maximum):", output[[1]][3], "\n"))
    cat(paste("--Weighted (minimum):", output[[1]][4], "\n"))
    cat("-Distance ")
    output[[2]] <- distance_w(net.b)
    cat(paste("(based on the giant component of ", nrow(output[[2]]), ")\n", sep=""))
    cat(paste("--Binary:            ", mean(output[[2]], na.rm=TRUE), "\n"))
    output[[3]] <- distance_w(net)
    cat(paste("--Weighted (unnorm.):", mean(output[[3]], na.rm=TRUE), "\n"))
    cat(paste("--Weighted (norm.):  ", mean(output[[3]] * mean(net[,3]), na.rm=TRUE), "\n\n"))
  } else {
    cat("    (skipped)\n")
  }
  cat("2: Calculating measures on random network\n")
  if(2 %in% step) {
    cat(paste("    (based on ", NR, " weight reshuffled networks)\n", sep=""))
    output[[4]]<- matrix(data=0, nrow=NR, ncol=9)
    cat(paste("0%  10%  20%  30%  40%  50%  60%  70%  80%  90%  100%\n",
              "+----+----+----+----+----+----+----+----+----+----+\n|", sep=""));
    #Preform loop of all ties
    for (t in 1:NR) {
      if(indicator[t]) cat("|")
      net.r <- rg_reshuffling_w(net, option="weights", directed=directed, seed=t)
      output[[4]][t,1:5] <- clustering_w(net.r, measure=c("am", "gm", "ma", "mi","bi"))
      tmp <- distance_w(net.r)
      output[[4]][t,7] <- mean(tmp, na.rm=TRUE)
      output[[4]][t,8] <- mean(tmp * mean(net[,3]), na.rm=TRUE)
      output[[4]][t,9] <- nrow(tmp)
    }
    cat("\n")
    output[[4]][,6] <- mean(output[[2]], na.rm=TRUE)
    randomw.output <- colMeans(output[[4]])
    cat("-Clustering coefficient\n")
    cat(paste("--Binary:            ", randomw.output[5], "\n"))
    cat(paste("--Weighted (a mean): ", randomw.output[1], "\n"))
    cat(paste("--Weighted (g mean): ", randomw.output[2], "\n"))
    cat(paste("--Weighted (maximum):", randomw.output[3], "\n"))
    cat(paste("--Weighted (minimum):", randomw.output[4], "\n"))
    cat(paste("-Distance (based on giant components of", randomw.output[9], "nodes on average)\n"))
    cat(paste("--Binary:            ", randomw.output[6], "\n"))
    cat(paste("--Weighted (unnorm.):", randomw.output[7], "\n"))
    cat(paste("--Weighted (norm.):  ", randomw.output[8], "\n\n"))
  } else {
    cat("    (skipped)\n")
  }
  cat("3: Calculating measures on random network\n")
  if(3 %in% step) {
    cat(paste("    (based on ", NR, " link reshuffled networks)\n", sep=""))
    output[[5]] <- matrix(data=0, nrow=NR, ncol=9)
    cat(paste("0%  10%  20%  30%  40%  50%  60%  70%  80%  90%  100%\n",
              "+----+----+----+----+----+----+----+----+----+----+\n|", sep=""));
    #Preform loop of all ties
    for (t in 1:NR) {
      if(indicator[t]) cat("|")
      net.r <- rg_reshuffling_w(net, option="links", directed=directed, seed=t)
      output[[5]][t,1:5] <- clustering_w(net.r, measure=c("am", "gm", "ma", "mi","bi"))
      tmp <- distance_w(dichotomise(net.r))
      output[[5]][t,6] <- mean(tmp, na.rm=TRUE)
      tmp <- distance_w(net.r)
      output[[5]][t,7] <- mean(tmp, na.rm=TRUE)
      output[[5]][t,8] <- mean(tmp * mean(net[,3]), na.rm=TRUE)
      output[[5]][t,9] <- nrow(tmp)
    }
    cat("\n")
    randoml.output <- colMeans(output[[5]])
    cat("-Clustering coefficient\n")
    cat(paste("--Binary:            ", randoml.output[5], "\n"))
    cat(paste("--Weighted (a mean): ", randoml.output[1], "\n"))
    cat(paste("--Weighted (g mean): ", randoml.output[2], "\n"))
    cat(paste("--Weighted (maximum):", randoml.output[3], "\n"))
    cat(paste("--Weighted (minimum):", randoml.output[4], "\n"))
    cat(paste("-Distance (based on giant components of", randoml.output[9], "nodes on average)\n"))
    cat(paste("--Binary:            ", randoml.output[6], "\n"))
    cat(paste("--Weighted (unnorm.):", randoml.output[7], "\n"))
    cat(paste("--Weighted (norm.):  ", randoml.output[8], "\n"))
  } else {
    cat("    (skipped)\n")
  }
  return(output)
}

