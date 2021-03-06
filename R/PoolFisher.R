library(roxygen2)
library(devtools)

#' Fisher Test Meta Analysis
#'
#'This function uses the Fisher test to pool pvalues
#'It sums up the log-transformed p-values obtained from individual studies
#'It follows a chisquare distribution with 2k degrees of freedom under the null hypothesis
#'
#' @param pvalues is a list of pvalues
#'
#' @return the Fisher pooled p-value as the test statistic
#'
#' @examples
#' pvalues<- c(0.486667826,0.154255057, 0.858419083, 0.486667826 ,0.405013316, 0.316372249, 0.173853520, 0.284391656, 0.370577866, 0.421579844, 0.221750129, 0.962561630, 0.392123661, 0.127102942, 0.896587161, 0.665359191)
#' pool.fisher(pvalues)


pool.fisher <- function(pvalues){
  k <- length(pvalues)
  df <- 2*k
  TS <- -2*sum(log(pvalues))
  p.value <- pchisq(TS, df=df,lower.tail=F)
  return(p.value)
}