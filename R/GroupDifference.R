library("devtools")
library(roxygen2)

#' Pooled test
#'
#' @param x is a data frame, can be normally distributed or not normally distributed. For 2 groups we will perform a two sample t-test or Wilcoxon rank sum test. For more than 2 groups we will perform ANOVA or Kruskal Wallis test.  
#'
#' @return A list containing p values
#'
#' @examples
#' set.seed(123)
#' p <- 100
#' data1 <- data.frame(group=sample(1:3,200,replace=TRUE),matrix(rnorm(p*200),ncol=p))
#' GroupDifference(data1)


GroupDifference = function(x){
  p = length(x)
  n = length(x[,1])
  groups = nrow(unique(x[1]))
  p_values = rep(NA,p-1)
  #ANOVA Code
  if(groups>2){  
    #test normality
    for(i in 2:p){
      y = data.frame(group = x[,1], biomarker = x[,i])
      normal = TRUE
      for(j in 1:groups){
        y_j = y[which(y$group==j),][,2]
        pvalue = shapiro.test(y_j)[[2]]
        if(pvalue<=.05){
          normal=FALSE
          break
        }
      }
      
      #for normal
      if(normal){
        fit = lm(y$group~y$biomarker)
        pvalue = anova(fit)[,5][1]
      }
      
      #for not normal
      else{
        pvalue = kruskal.test(y$group~y$biomarker)[3]$p.value
      }
      p_values[i-1] = pvalue
    }
    return(p_values)
  }
  #Two Samp T Test Code
  else{
    for(i in 2:p){
      y = data.frame(group = x[,1], biomarker = x[,i])
      y1 = y[which(y$group==1),][,2]
      y2 = y[which(y$group==2),][,2]
      
      #check for normal
      norm_p_y1 = shapiro.test(y1)$p
      norm_p_y2 = shapiro.test(y2)$p
      #for normal
      if(norm_p_y1>.05 & norm_p_y2>.05){
        var_p = var.test(y1,y2)[3]$p.value
        #equal variance
        if(var_p>.05){
          pvalue = t.test(y1,y2,var.equal = T)[3]$p.value
        }
        #unequal variance
        else{
          pvalue = t.test(y1,y2,var.equal = F)[3]$p.value
        }
      }
      
      #for not normal
      else{
        pvalue = wilcox.test(y1,y2)[3]$p.value
      }
      p_values[i-1] = pvalue
    }
  }
  return(p_values)
}
