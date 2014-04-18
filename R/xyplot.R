
xyplot.simplejags <- function(x){
  devAskNewPage(ask=FALSE)
  xyplot(x$samples)
}