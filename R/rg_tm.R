`rg_tm` <-
function(ni=100,np=100,ties=300,max.weight=1,seed=NULL){
  #If seed is set, set if formally
  if (!is.null(seed))
    set.seed(as.integer(seed))
  #Create random edgelist object with 5% more ties than needed
  net <- data.frame(i=sample(1:ni, (ties*1.5), replace=TRUE), 
                    p=sample(1:np, (ties*1.5), replace=TRUE),
                    w=sample(1:max.weight, (ties*1.5), replace=TRUE))
  #Remove duplicated entries
  net <- net[!duplicated(net[,1:2]),]
  net <- net[1:ties,]
  if(max.weight==1) {
    return(as.tnet(net[,1:2], type="binary two-mode tnet"))
  } else {
    return(as.tnet(net, type="weighted two-mode tnet"))
  }
}