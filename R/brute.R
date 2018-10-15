
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
  which.max(wtta400)

  itta <- sapply(c(1:length(weight3)),
                 function(x) {
                   (combn(length(weight3),x))
                 })

  k <- c(itta[[2]][49],itta[[2]][50])
  brute<-list()
  brute[["value"]] = round(max(wtta300*vtta200),0)
  brute[["elements"]] = k
  brute

  return(brute)
}
