`weighted_richclub_w` <-
function(net,rich="k", reshuffle="weights", samples=100, seed=NULL, directed=NULL){
  if(is.null(attributes(net)$tnet))
    net <- as.tnet(net, type="weighted one-mode tnet")
  if(attributes(net)$tnet!="weighted one-mode tnet")
    stop("Network not loaded properly")
  #Internal function: the non-normalised coefficient
  `weighted_richclub_internal` <-
  function(net,club="degree",directed=FALSE){
    #Name columns
    dimnames(net)[[2]] <- c("i", "j", "w")
    #Order net
    net <- net[order(net[,"i"],net[,"j"]),]
    #Calculate prominence vector
    k.list <- degree_w(net, measure=club[2:length(club)]);
    if(club[1]=="avg.w")
      k.list <- cbind(k.list, 
                      avg.w=k.list[,'output']/k.list[,'degree'])
    #Defining x-axis
    output <- unique(k.list[,club[1]])
    output <- cbind(x=output[order(output)],num=NaN,den=NaN,y=NaN)
    net <- cbind(net, rc.i=rep(k.list[,club[1]],
                  k.list[,"degree"]))
    if(directed==FALSE) {
      net <- net[order(net[,"j"],net[,"i"]),]
      net <- cbind(net, rc.j=rep(k.list[,club[1]],
        k.list[,"degree"]))
      net <- net[order(net[,"i"],net[,"j"]),]
    } else {
      net <- cbind(net, rc.j=sapply(net[,"j"], 
        function(a) sum(k.list[k.list[,"vertex"]==a,club[1]])))
    }
    net <- cbind(net, 
      rc=pmin.int(net[,"rc.i"],net[,"rc.j"]))
    output[,"num"] <- sapply(output[,"x"], 
      function(a) sum(net[net[,"rc"]>=a,"w"]))
    tmp.no.edges <- sapply(output[,"x"], 
      function(a) length(net[net[,"rc"]>=a,"w"]))
    net <- net[order(-net[,"w"]),]
    output[,"den"] <- sapply(output[,"x"], function(a) 
      sum(net[1:tmp.no.edges[which(output[,"x"]==a)],"w"]))
    output[,"y"] <- output[,"num"]/output[,"den"]
    return(output)
  }
  #If seed is set, formally set it
  if(!is.null(seed))
    set.seed(as.integer(seed))
  #Check whether the network is directed
  if(is.null(directed))
    directed <- (nrow(symmetrise(net))!=nrow(net))
  #Get the measure parameter for degree_w
  if(rich=="k") {
    club <- c("degree","degree")
  } else if(rich=="s") {
    club <- c("output","degree")
  } else if(rich=="w") {
    club <- c("avg.w","degree","output")
  } else {
    stop("rich must be 'k', 's', or 'w'")
  }
  #Calculate the non-normalised coefficient
  table <- weighted_richclub_internal(net=net, club=club, 
             directed=directed)
  observed <- matrix(data=NaN, nrow=max(table[,"x"])+1, ncol=3)
  row.names(observed)<- 0:max(table[,"x"])
  dimnames(observed)[[2]]<- c("obs", "rdm", "rate")
  for (j in table[,"x"]) 
    observed[j+1,"obs"]<-table[which(table[,"x"]==j),"y"]
  #Create table of random network results
  random.m <- matrix(data=NaN, nrow=samples, ncol=nrow(observed))
  #Random network loop
  for (i in 1:samples) {
    #Create random network
    rnet <- rg_reshuffling_w(net=net, option=reshuffle, directed=directed)
    #Calculate the non-normalised coefficient
    table <- weighted_richclub_internal(net=rnet, club=club, directed=directed)
    #Put results in table
    for (j in table[,"x"])
      random.m[i,j+1]<-table[which(table[,"x"]==j),"y"]
    #Show indicator
    if(i/10 == round(i/10))
      cat(paste("Random network ", i, "/", samples, " @ ",
        date(), "\n", sep=""));
  }
  #Get average from the random networks
  observed[,"rdm"] <- colMeans(random.m)
  #Divide observed by random
  observed[,"rate"] <- observed[,"obs"]/observed[,"rdm"]
  observed <- observed[!is.na(observed[,"obs"]),]
  observed <- observed[!is.na(observed[,"rdm"]),]
  observed <- observed[!is.na(observed[,"rate"]),]
  return(observed)
}