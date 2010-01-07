`rg_tm` <-
function(ni=100,np=100,ties=300,max.weight=1,seed=NULL){
  #If seed is set, set if formally
  if (!is.null(seed))
    set.seed(as.integer(seed))
  if(ties > 1) {
    #Create random edgelist object with 50% more ties than needed
    net <- data.frame(i=sample(1:ni, (ties*1.5), replace=TRUE), 
                      p=sample(1:np, (ties*1.5), replace=TRUE))
    #Remove duplicated entries
    net <- net[!duplicated(net[,1:2]),]
    net <- net[1:ties,]
  } else {
    # Create a random edgelist object based on probabilities
    net <- which(matrix(data=runif(ni*np), nrow=ni, ncol=np)<ties, arr.ind=TRUE)
  }
  if(max.weight==1) {
    return(as.tnet(net[,1:2], type="binary two-mode tnet"))
  } else {
    return(as.tnet(data.frame(net, w=sample(1:max.weight, nrow(net), replace=TRUE)), type="weighted two-mode tnet"))
  }
}