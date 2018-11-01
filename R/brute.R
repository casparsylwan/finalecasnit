#' Tests Knapsack using Brute Force Search method
#'
#' @param x The first parameter i.e. a data frame
#' @param W The second parameter i.e. weight constraint
#'
#' @return
#' @export
#' @importFrom utils combn
#'
brute_force_knapsack <- function(x,W){
  if(!is.data.frame(x)==TRUE){
    stop("The input is not a data frame")
  }
  if(W<1){stop("W has to be posstive!")}

  weight1 <- as.list(x[1])
  weight2 <- unlist(weight1)
  weight3 <- as.numeric(weight2)


  value1 <- as.list(x[2])
  value2 <- unlist(value1)
  value3 <- as.numeric(value2)

  #Weights: Conversion of all possible combinations from List to Vector
  wtta <- sapply(c(1:length(weight3)),
                 function(balta) {
                   colSums(combn(weight3,balta))
                 })
  wtta100 <- unlist(wtta)
  wtta200 <- as.vector(wtta100)

  #Values: Conversion of all possible combinations from List to Vector
  vtta <- sapply(c(1:length(value3)),
                 function(yalta) {
                   colSums(combn(value3,yalta))
                 })

  vtta100 <- unlist(vtta)
  vtta200 <- as.vector(vtta100)
  #Logical condition and identify max possible value
  wtta300 <- wtta200 <=W
  wtta400 <- wtta300*vtta200
  ant <-  which.max(wtta400) #position of weight combination
  #print(ant)
  barebone <- sapply(c(1:length(x[,1])),function(kb1){ combn(length(x[,1]),kb1)})
  #print(barebone)

  ant1 <-ifelse(ant<length(barebone[[1]]),paste("L1",ant),
                ifelse(ant<(length(barebone[[1]])+length(barebone[[2]])/2),paste("L2",ant-(length(barebone[[1]]))),
                       ifelse(ant<(length(barebone[[1]])+length(barebone[[2]])/2+length(barebone[[3]])/3),paste("L3",ant-(length(barebone[[1]])+length(barebone[[2]])/2)),0)))
  #print("ant1");print(ant1)

  anton <-0
  if("L1" %in% substr(ant1,1,2)){
    anton <-(barebone[[1]][,as.numeric(noquote(substr(ant1,4,5)))])
  } else
    if("L2" %in% substr(ant1,1,2)){
      anton <-(barebone[[2]][,as.numeric(noquote(substr(ant1,4,5)))])
    } else
      if("L3" %in% substr(ant1,1,2)){
        anton <-(barebone[[3]][,as.numeric(noquote(substr(ant1,4,5)))])
      }
  #print("anton");print(anton)

  brute<-list()
  brute[["value"]] = round(max(wtta300*vtta200),0)
  brute[["elements"]] = anton
  brute
  return(brute)
}
