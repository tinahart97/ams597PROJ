library(roxygen2)
library(devtools)

#' Min Test Meta Analysis
#'
#'
#'It follows a beta distribution with degrees of freedom alpha=1 and beta=k under the null hypothesis

#' @param input is a list of p-values

#' @return the minimum p-value among the K studies as the test statistic 
#'
#' @examples
#' pvalues<- c(0.486667826,0.154255057, 0.858419083, 0.486667826 ,0.405013316, 0.316372249, 0.173853520, 0.284391656, 0.370577866, 0.421579844, 0.221750129, 0.962561630, 0.392123661, 0.127102942, 0.896587161, 0.665359191)
#' pool.min(pvalues)


pool.min <- function(pvalues){
  k <- length(pvalues)
  TS <- min(pvalues)
  p.value <- pbeta(TS, shape1=1, shape2=k, lower.tail=F)
  return(p.value)
}