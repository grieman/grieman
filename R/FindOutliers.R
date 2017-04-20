#' FindOutliers
#'
#' This function identifies time series outliers in a data signal
#'
#' @param x a numeric vector or time series to be processed
#' @param IQRs Allowable number of IQRs' away from the first or third quartile for a point to not be considered an outlier. Higher numbers result in fewer outliers.
#' @param plot Enables a plot highlighting detected outliers
#'
#' @return A vector with likely weights of outliers.
#' @export
#'
#' @examples
#' FindOutliers(c(2,1,2,2,0,2,20000,2,2,1,2))
#' FindOutliers(c(1,2,3,5,-10,4,2,4,2,6,8,9), plot=TRUE)
#'
FindOutliers <- function(x,IQRs=3, plot=FALSE){
  x <- stats::as.ts(x)
  if(stats::frequency(x)>1)
    resid <- stats::stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
  else
  {
    tt <- 1:length(x)
    resid <- stats::residuals(stats::loess(x ~ tt, na.action=stats::na.exclude))
  }
  resid.q <- stats::quantile(resid,prob=c(0.25,0.75), na.rm=TRUE)
  iqr <- diff(resid.q)
  limits <- resid.q +IQRs*iqr*c(-1,1)
  score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0)) #0 if not an outlier, otherwise measures how much of an outlier
  if(plot){
    plot(x)
    x2 <- stats::ts(rep(NA,length(x)))
    x2[score>0] <- x[score>0]
    stats::tsp(x2) <- stats::tsp(x)
    graphics::points(x2,pch=19,col="red")
    return(invisible(score))
  }
  else
    return(score)
}
