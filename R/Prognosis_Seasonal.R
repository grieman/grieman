#' Prognosis_Seasonal
#'
#' @param data Input data, as a vector
#' @param times Vector of times for each data point
#' @param seasonality How many data point there are in each seasonal period
#' @param forecastLength Number of points to forecast - default is .2 of the input's length
#' @param forecastConfidence Level for plotted and output confidence interval
#' @param outlierTolerance Allowable number of IQRs' away from the first or third quartile for a point to not be considered an outlier. Higher numbers result in fewer outliers.
#' @param detailed Allows for more detailed output, where applicable. Defaults to FALSE
#'
#' @return A list consisting of a plot, a model, and predictions
#' @export
#'
#' @examples
#' exampledata <- GenerateSeriesToCutoff(100, .01, .005, 200, 100)
#' times <- seq(as.Date("2000/1/1"), by = "month", length.out = length(exampledata))
#' Prognosis_output<- Prognosis_Seasonal(exampledata, times, 12)
#' Prognosis_output$Plot
#' Prognosis_output$Model
#' Prognosis_output$Table
#'
Prognosis_Seasonal <- function(data, times, seasonality,forecastLength=NULL, forecastConfidence = 50, outlierTolerance = 2, detailed=FALSE){
  if(is.null(forecastLength)){forecastLength <- floor(length(data)*.05)}

  #### Clean data ####
  data[which(data==-5555)] <- NA

  outliers <- FindOutliers(data, IQRs=outlierTolerance)
  data[which(outliers > 0)] <- NA
  data <- as.numeric(zoo::na.approx(data))

  df <- data.frame(times, data)
  timediff1 <- lubridate::int_length(lubridate::interval(times[1], times[2]))
  timediff2 <- lubridate::int_length(lubridate::interval(times[length(times)-1], times[length(times)]))
  df1.zoo<-zoo::zoo(df[,-1],df[,1])
  df2 <- merge(df1.zoo,zoo::zoo(,seq(stats::start(df1.zoo),stats::end(df1.zoo),by=min(timediff1, timediff2))), all=TRUE)
  df <- data.frame(x=stats::time(df2), df2, row.names=NULL)

  datats <- stats::ts(data, frequency = seasonality)

  #### Fit Model ####
  model <- forecast::stlm(datats, method="arima")

  #### Create Model Information ####
  fut_times <- c(times[length(times)] + lubridate::seconds(lubridate::int_length(lubridate::interval(times[1], times[2]))))
  for (i in 2:forecastLength){
    fut_times[i] <- fut_times[i-1] + lubridate::seconds(lubridate::int_length(lubridate::interval(times[1], times[2])))
  }


  stl_forecasts <- forecast::forecast(model, forecastLength, level = c(forecastConfidence,80,95))
  forecast_means <- stl_forecasts$mean; forecast_high <- stl_forecasts$upper[,1]; forecast_low <- stl_forecasts$lower[,1]
  predictions <- data.frame(fut_times, rep(NA, forecastLength),forecast_means, forecast_high, forecast_low, stl_forecasts$upper[,2],stl_forecasts$lower[,2],stl_forecasts$upper[,3],stl_forecasts$lower[,3])
  colnames(predictions) <- c('times','data', 'forecasts',paste('high_CI',forecastConfidence, sep='_'), paste('low_CI',forecastConfidence, sep='_'),"high80", "low80","high95", "low95")

  data.df <- data.frame(times,data, as.numeric(stl_forecasts$fitted),c(rep(NA, (length(data)-1)),utils::tail(stl_forecasts$fitted,n=1)),c(rep(NA, (length(data)-1)),utils::tail(stl_forecasts$fitted,n=1)),
                        c(rep(NA, (length(data)))),c(rep(NA, (length(data)))),c(rep(NA, (length(data)))),c(rep(NA, (length(data)))))
  colnames(data.df) <-  c('times', 'data', 'forecasts',paste('high_CI',as.character(forecastConfidence), sep='_'), paste('low_CI',as.character(forecastConfidence), sep='_'),
                          "high80", "low80","high95", "low95")
  output <- rbind(data.df, predictions)

  #### Output Plot and Info ####
  plot <- ggplot2::ggplot(output,ggplot2::aes(x=times)) + ggplot2::geom_line(ggplot2::aes(y=data),col="#a7a9ac") + ggplot2::geom_line(ggplot2::aes(y=forecasts), col="#004990")+
    ggplot2::geom_ribbon(ggplot2::aes_string(ymin=paste('low_CI',forecastConfidence, sep='_'), ymax=paste('high_CI',forecastConfidence, sep='_')), alpha=.25)+theme_GR()

  if(detailed==TRUE){
    outlist <- list(plot,model,output,forecast::accuracy(data.df$forecasts,data.df$data))
    names(outlist) <- c("Plot", "Model", "Table", "Accuracy")
  } else {
    outlist <- list(plot,model,output)
    names(outlist) <- c("Plot", "Model", "Table")
  }

  class(outlist) <- append(class(outlist), "Prognosis")
  return(outlist)

}
