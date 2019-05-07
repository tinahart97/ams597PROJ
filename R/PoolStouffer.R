library(roxygen2)
library(devtools)

#'Stouffer Test Meta Analysis
#'
#'This function uses the Stouffer test to pool pvalues
#'It sums the inverse normal transformed p-values
#' follows a standard normal distribution under the null hypothesis 

#' @param input is a list of p-values

#' @return the Stouffer p-value as the test statistic
#'
#' @examples
#' pvalues<- c(0.486667826,0.154255057, 0.858419083, 0.486667826 ,0.405013316, 0.316372249, 0.173853520, 0.284391656, 0.370577866, 0.421579844, 0.221750129, 0.962561630, 0.392123661, 0.127102942, 0.896587161, 0.665359191)
#' pool.fisher(pvalues)

pool.stouffer <- function(pvalues){
  k <- length(pvalues)
  TS <- sum(qnorm(pvalues,lower.tail=F)) / sqrt(k)
  p.value <- pnorm(TS, lower.tail=F)
  return(p.value)
}

