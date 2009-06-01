`weighted_richclub` <-
function(edgelist,rich="k", reshuffle="weights", samples=100, 
seed=NULL, directed=NULL){
  #Internal function: the non-normalised coefficient
  `weighted_richclub_internal` <-
  function(edgelist,club="degree",directed=FALSE){
    #Name columns
    dimnames(edgelist)[[2]] <- c("i", "j", "w")
    #Order edgelist
    edgelist <- edgelist[order(edgelist[,"i"],edgelist[,"j"]),]
    #Calculate prominence vector
    k.list <- degree_w(edgelist, measure=club[2:length(club)]);
    if(club[1]=="avg.w")
      k.list <- cbind(k.list, 
                      avg.w=k.list[,'output']/k.list[,'degree'])
    #Defining x-axis
    output <- unique(k.list[,club[1]])
    output <- cbind(x=output[order(output)],num=NaN,den=NaN,y=NaN)
    edgelist <- cbind(edgelist, rc.i=rep(k.list[,club[1]],
                  k.list[,"degree"]))
    if(directed==FALSE) {
      edgelist <- edgelist[order(edgelist[,"j"],edgelist[,"i"]),]
      edgelist <- cbind(edgelist, rc.j=rep(k.list[,club[1]],
        k.list[,"degree"]))
      edgelist <- edgelist[order(edgelist[,"i"],edgelist[,"j"]),]
    } else {
      edgelist <- cbind(edgelist, rc.j=sapply(edgelist[,"j"], 
        function(a) sum(k.list[k.list[,"vertex"]==a,club[1]])))
    }
    edgelist <- cbind(edgelist, 
      rc=pmin.int(edgelist[,"rc.i"],edgelist[,"rc.j"]))
    output[,"num"] <- sapply(output[,"x"], 
      function(a) sum(edgelist[edgelist[,"rc"]>=a,"w"]))
    tmp.no.edges <- sapply(output[,"x"], 
      function(a) length(edgelist[edgelist[,"rc"]>=a,"w"]))
    edgelist <- edgelist[order(-edgelist[,"w"]),]
    output[,"den"] <- sapply(output[,"x"], function(a) 
      sum(edgelist[1:tmp.no.edges[which(output[,"x"]==a)],"w"]))
    output[,"y"] <- output[,"num"]/output[,"den"]
    return(output)
  }
  #If seed is set, formally set it
  if(!is.null(seed))
    set.seed(as.integer(seed))
  #Check whether the network is directed
  if(is.null(directed))
    directed <- (nrow(symmetrise(edgelist))!=nrow(edgelist))
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
  table <- weighted_richclub_internal(edgelist=edgelist, club=club, 
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
    edgelist <- rg_reshuffling_w(edgelist=edgelist, option=reshuffle, 
                  directed=directed)
    #Calculate the non-normalised coefficient
    table <- weighted_richclub_internal(edgelist=edgelist, club=club, 
               directed=directed)
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