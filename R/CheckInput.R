library(roxygen2)
library(devtools)

#' Check Input for Pooling Data
#'
#' @param frames is a list of data frames
#'
#' @examples
#' set.seed(123)
#' p <- 100
#' data1 <- data.frame(group=sample(1:3,200,replace=TRUE),matrix(rnorm(p*200),ncol=p))
#' data2 <- data.frame(group=sample(1:2,150,replace=TRUE),matrix(rnorm(p*150),ncol=p))
#' frames <- list(data1, data2)
#' check.input(frames)


check.input <- function(frames){
  # frames: list
  n_cols <- ncol(frames[[1]])
  col_labels <- colnames(frames[[1]])
  
  for (f in frames){
    # Check that a list of data frames are being entered.
    if (typeof(f) != "list"){
      stop("list contains elements not of type list")
    }
    
    # Check that number of columns are the same.
    n_c <- ncol(f)
    if (n_c != n_cols){
      stop("not all data frames have the same number of columns")
    }
    
    # Check that all column names are the same.
    if (any(colnames(f) != col_labels)){
      stop("there exists a column with an incorrect label")
    }
  }
  for (i in 1:length(frames)){
    if((length(unique(frames[[i]][,1]))<2)== TRUE){
      stop("Groups are not unique in a Data Frame")
    }
  }
}




