library("devtools")
library(roxygen2)

#' Pooling Data
#'
#'This function uses different tests (Fisher, Stouffer, Min, Max) to pool data from different data frames.
#' @param x1, x2, x3, x4, x5 are data frames with each data frame consisting of identical number of columns. Must input at least 2 and at most 5 data frames.
#' @param test indicates which pooling test to use, must be either fisher, stouffer, min, max

#' @return P values for the input test
#'
#' @examples
#' set.seed(123)
#' p <- 100
#' data1 <- data.frame(group=sample(1:3,200,replace=TRUE),matrix(rnorm(p*200),ncol=p))
#' data2 <- data.frame(group=sample(1:2,150,replace=TRUE),matrix(rnorm(p*150),ncol=p))
#' Project(x1= data1, x2= data2, test= "fisher")


Project = function(x1,x2,x3=NULL,x4=NULL,x5=NULL,test=c("fisher","stouffer","min","max")){
  
  #check if enough data freames were entered
  if(is.null(x1) | is.null(x2)){
    stop("Not enough data frames entered")
  }
  
  #get the p_value lists for each biomarker for each data frame
  P_x1 = NULL
  P_x2 = NULL
  P_x3 = NULL
  P_x4 = NULL
  P_x5 = NULL
  
  x = list()
  
  if (!is.null(x1)){
    P_x1 = GroupDifference(x1)
    x[[1]] = x1
  }
  if (!is.null(x2)){
    P_x2 = GroupDifference(x2)
    x[[2]] = x2
  }
  if (!is.null(x3)){
    P_x3 = GroupDifference(x3)
    x[[3]] = x3
  }
  if (!is.null(x4)){
    P_x4 = GroupDifference(x4)
    x[[4]] = x4
  }
  if (!is.null(x5)){
    P_x5 = GroupDifference(x5)
    x[[5]] = x5
  }
  
  check.input(x)
  
  data = rbind(P_x1,P_x2,P_x3,P_x4,P_x5)
  #or colbind bc we have no clue what you want
  data = data.frame(data)
  
  #initiate the p values we want to find
  if(test == "fisher"){
    x = sapply(data, pool.fisher)
  }
  else if(test == "stouffer"){
    x = sapply(data, pool.stouffer)
  }
  else if(test == "min"){
    x = sapply(data, pool.min)
  }
  else if(test == "max"){
    x = sapply(data, pool.max)
  }
  return(x)
}


