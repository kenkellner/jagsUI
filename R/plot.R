
plot.simplejags <- function(x){
  devAskNewPage(ask=FALSE)
  plot(x$samples)
}