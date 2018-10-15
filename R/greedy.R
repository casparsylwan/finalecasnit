#GREEDY HEURISTIC OF KNAPSACK
#' Tests Knapsack using Greedy Heuristic method
#'
#' @param x The first parameter i.e. a data frame
#' @param W The second parameter i.e. weight constraint
#'
#' @return
#' @export
#'
#'
greedy_knapsack <- function(x ,W){
  if(!is.data.frame(x)==TRUE){
    stop("The input is not a data frame")
  }
  weight1 <- as.list(x[1])
  weight2 <- unlist(weight1)
  weight3 <- as.numeric(weight2)
  value1 <- as.list(x[2])
  value2 <- unlist(value1)
  value3 <- as.numeric(value2)

  #combined Weight and value data.frame
  df <- data.frame(weight3,value3)
  #Process for new variable
  wtta300 <- value3/weight3
  df["ratvalwt"] <- wtta300
  df_decrease <- df[order(df[,'ratvalwt'],decreasing = TRUE )[1:nrow(df)],]
  #Function to calculate greedy heuristic
  wag <- 0
  i <- 1
  greed_value <- 0
  greed_elements <- c()

  while (wag < W || i<=n) {
    if(df_decrease$weight3[i] <= W-wag){
      wag <- wag + df_decrease$weight3[i]

      greed_value <- greed_value + df_decrease$value3[i]
      greed_elements <- append(greed_elements,rownames(df_decrease[i,]))
      i <- i+1
    }
    else{
      break()
    }
  }

  k <- noquote(greed_elements)
  greedy<-list()
  greedy[["value"]] = round(greed_value,0)
  greedy[["elements"]] = k
  return(greedy)
}


