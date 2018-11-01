---
title: "Tests Knapsack using four different methods"
author: "Caspar, Nitin"
date: "2018-11-01"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Tests Knapsack using four different methods}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
The package tests Knapsack using four difference methods.

## Vignette Info

finalecasnit package tests Knapsack using Brute Force Search, Dynamic Programming, Greedy Heuristic, and Rcpp.

## Brute Force Search


```r
library(finalecasnit)
set.seed(42) #Given data
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
```

```
## $value
## [1] 16770
## 
## $elements
## [1] 5 8
```

## Greedy Heuristic


```r
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
```

```
## $value
## [1] 192647
## 
## $elements
##  [1] 92  574 472 80  110 537 332 117 37  776 577 288 234 255 500 794 55 
## [18] 290 436 346 282 764 599 303 345 300 243 43  747 35  77  229 719 564
```
