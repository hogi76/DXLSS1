#'poststep1 - Control chart for post management (I-MR)
#'
#' @param x : i-mr control chart data (individual data)
#' @return : i-mr control chart
#' @examples  poststep1(pss5$throw_Distance)


poststep1=function(x){
  par(mfrow=c(1,1))
  qcc(x,type='xbar.one')
  x2=cbind(x[1:length(x)-1],x[2:length(x)])
  qcc(x2,type="R")
}




#'poststep2 - Control chart for post management (Xbar-R)
#'
#' @param x : Xbar-R control chart data (subgroub size=4~5)
#' @return : Xbar-R control chart
#' @examples  poststep2(pss6)


poststep2=function(x){
  par(mfrow=c(1,1))
  qcc(x,type='xbar')
  qcc(x,type="R")
}
