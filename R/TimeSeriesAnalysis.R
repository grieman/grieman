#' TimeSeriesAnalysis
#'
#' Detects and marks outliers, breakpoints, and changes in variance in a time series.
#'
#' @param valuecol Data vector
#' @param datecol Date vector
#' @param IQRs Allowable number of IQRs' away from the first or third quartile for a point to not be considered an outlier. Higher numbers result in fewer outliers.
#' @param plot toggles drawing of plot
#' @param plotly toggles plotting in plotly or ggplot
#'
#' @return a data frame with the times, data values, outliers, changes in mean, and changes in variance
#' @export
#'
#' @examples
#' data <- c(rnorm(50,0,1), rnorm(50,0,2),20,rnorm(50,10,1))
#' dates <- seq(as.Date("2016/1/1"), by="days", length.out=151)
#' TimeSeriesAnalysis(data,dates)
#'
#' TimeSeriesAnalysis(c(1,2,3,5,-10,4,2,4,2,6,8,9), seq(as.Date("2016/1/1"), by="days", length.out=12))

TimeSeriesAnalysis <- function(valuecol, datecol, plot=TRUE, plotly=FALSE, IQRs=2){
  valuecol <- valuecol[order(datecol)]
  datecol <- datecol[order(datecol)]

  xts <- xts::xts(valuecol, datecol)
  xts <- zoo::na.approx(xts)
  outliers <- FindOutliers(xts,IQRs, plot=FALSE)
  means <- changepoint::cpt.mean(stats::as.ts(xts), method="PELT")
  vars <- changepoint::cpt.var(stats::as.ts(xts))

  dfout <- data.frame(datecol, valuecol, outliers, rep(0, length(outliers)), rep(0,length(outliers)))
  colnames(dfout) <- c("time","value","outliers","meanchange","varchange")
  dfout$meanchange[means@cpts] <- 1
  dfout$varchange[vars@cpts] <- 1

  if (plot == TRUE){
    plot <- ggplot2::ggplot(dfout, ggplot2::aes(x=time, y=value)) + ggplot2::geom_line(ggplot2::aes(alpha=.15)) +
      ggplot2::geom_point(ggplot2::aes(x=time, y=value, color = "Outlier", alpha=ifelse(outliers > 0, .75, 0))) +
      ggplot2::geom_point(ggplot2::aes(x=time, y=value, color = "Variance Change", alpha=ifelse(varchange > 0, .5, 0))) +
      ggplot2::geom_point(ggplot2::aes(x=time, y=value, color = "Mean Change", alpha=ifelse(meanchange > 0, .5, 0))) +
      ggplot2::scale_alpha_continuous(range=c(0,1),guide="none") + theme_GR()
    if (plotly==TRUE){
      print(plotly::ggplotly(plot))
    }else{
      print(plot)
    }

  }
  return(dfout)
}
