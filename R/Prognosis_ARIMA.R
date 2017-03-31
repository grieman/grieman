#' Prognosis_ARIMA
#'
#' This function predicts future values using {\href{https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average}{ARIMA}} and {\href{https://en.wikipedia.org/wiki/Autoregressive_conditional_heteroskedasticity#GARCH}{GARCH}} models.
#'
#' @param data Input data, as a vector
#' @param maxp Maximum number of AR terms
#' @param maxq Maximum number of MA terms
#' @param maxd Maximum number of differencing
#' @param tendtomean Chooses whether or not to include a mean in the ARIMA model
#' @param forecastLength number of points to forecast - default is .2 of the input's length
#' @param forecastConfidence Level for plotted and output confidence interval
#' @param outlierTolerance Allowable number of IQRs' away from the first or third quartile for a point to not be considered an outlier. Higher numbers result in fewer outliers.
#' @param detailed Allows for more detailed output, where applicable. Defaults to FALSE
#'
#' @return A list consisting of a plot, a model, and predictions
#' @export
#'
#' @examples
#' exampledata <- GenerateSeriesToCutoff(100, .01, .005, 200, 100)
#' Prognosis_output <- Prognosis_ARIMA(exampledata)
#' plotly::ggplotly(Prognosis_output$Plot)
#' Prognosis_output$Model
#' Prognosis_output$Table
Prognosis_ARIMA <- function(data, maxp=5, maxq=5, maxd=2, tendtomean=FALSE, forecastLength=NULL, forecastConfidence=50, outlierTolerance=2, detailed=FALSE){
  if(is.null(forecastLength)){forecastLength <- floor(length(data)*.05)}

  #### Clean data ####
  data[which(data==-5555)] <- NA

  outliers <- FindOutliers(data, IQRs=outlierTolerance)
  data[which(outliers > 0)] <- NA
  data <- as.numeric(zoo::na.approx(data))

  #### Fit ARIMA ####
  arimamodel <- forecast::auto.arima(data, max.p = maxp, max.q = maxq, max.d = maxd, allowmean = tendtomean)

  #### Check for ARCH ####
  Boxp <- stats::Box.test((arimamodel$residuals)^2, lag=12, type = "Ljung-Box")$p.value

  #### Fit GARCH if necessary ####
  if (Boxp < .05){
    garchmodel <- tseries::garch(arimamodel$residuals[!is.na(arimamodel$residuals)], order = c(1, 1),trace = F) #fits best garch model
  }

  #### Create Model Information ####
  if (Boxp < .05){
    last_residuals <- arimamodel$residuals[length(arimamodel$residuals) - ((garchmodel$order[2] - 1):0)] #last best_arch residuals
    deviations <- c()
    std_dev <- sqrt(sum(garchmodel$coef[(garchmodel$order[2] + 1):2] * (last_residuals)^2) +
                      garchmodel$coef[1] + garchmodel$coef[garchmodel$order[2] + 2] * arimamodel$sigma2) #uses arch to predict one-step-ahead variance
    deviations[1] <- std_dev
    last_residuals[1 + garchmodel$order[2]] <- 0
    for (k in 2:forecastLength) {
      std_dev <- sqrt(sum(garchmodel$coef[(garchmodel$order[2] + 1):2] * (last_residuals[k:length(last_residuals)])^2) +
                        garchmodel$coef[1] + garchmodel$coef[garchmodel$order[2] + 2] * (deviations[k-1] ^ 2)) #uses arch to predict one-step-ahead variance
      deviations[k] <- std_dev
      last_residuals[k + garchmodel$order[2]] <- 0 #future error goes here
      k <- k+1
    }#forecast errors using ARCH. No way to get future errors?
    arima_forecasts <- forecast::forecast(arimamodel, forecastLength, level = 0)
    forecast_means <- arima_forecasts$mean; confLevel <- (forecastConfidence/100) + (1-(forecastConfidence/100))/2
    forecast_high <- arima_forecasts$mean + stats::qnorm(confLevel) * sqrt(cumsum(deviations ^ 2));forecast_low <- arima_forecasts$mean + stats::qnorm(1-confLevel) * sqrt(cumsum(deviations ^ 2))
    high80 <- arima_forecasts$mean + stats::qnorm(.9) * sqrt(cumsum(deviations ^ 2));   low80 <- arima_forecasts$mean + stats::qnorm(.1)  * sqrt(cumsum(deviations ^ 2))
    high95 <- arima_forecasts$mean + stats::qnorm(.975) * sqrt(cumsum(deviations ^ 2)); low95 <- arima_forecasts$mean + stats::qnorm(.025) * sqrt(cumsum(deviations ^ 2))
    predictions <- data.frame((length(data)+1):(length(data) + forecastLength), rep(NA, forecastLength),as.numeric(forecast_means), as.numeric(forecast_high), as.numeric(forecast_low), as.numeric(high80), as.numeric(low80), as.numeric(high95), as.numeric(low95))
    colnames(predictions) <- c('index','data', 'forecasts',paste('high_CI',as.character(forecastConfidence), sep='_'), paste('low_CI',as.character(forecastConfidence), sep='_'),
                               "high80", "low80","high95", "low95")
    model <- garchmodel
  }else{
    arima_forecasts <- forecast::forecast(arimamodel, forecastLength, level = c(forecastConfidence,80,95))
    forecast_means <- arima_forecasts$mean; forecast_high <- arima_forecasts$upper[,1]; forecast_low <- arima_forecasts$lower[,1]

    predictions <- data.frame((length(data)+1):(length(data) + forecastLength),rep(NA, forecastLength),as.numeric(arima_forecasts$mean), as.numeric(arima_forecasts$upper[,1]), as.numeric(arima_forecasts$lower[,1]),as.numeric(arima_forecasts$upper[,2]),as.numeric(arima_forecasts$lower[,2]),
                              as.numeric(arima_forecasts$upper[,3]),as.numeric(arima_forecasts$lower[,3]))
    colnames(predictions) <- c('index','data', 'forecasts',paste('high_CI',as.character(forecastConfidence), sep='_'), paste('low_CI',as.character(forecastConfidence), sep='_'),
                               "high80", "low80","high95", "low95")
    model <- arimamodel
  }

  #### Output Plot and Info ####
  data.df <- data.frame(1:length(data),data, as.numeric(arima_forecasts$fitted),c(rep(NA, (length(data)-1)),utils::tail(arima_forecasts$fitted,n=1)),c(rep(NA, (length(data)-1)),utils::tail(arima_forecasts$fitted,n=1)),
                        c(rep(NA, (length(data)))),c(rep(NA, (length(data)))),c(rep(NA, (length(data)))),c(rep(NA, (length(data)))))
  colnames(data.df) <-  c('index', 'data', 'forecasts',paste('high_CI',as.character(forecastConfidence), sep='_'), paste('low_CI',as.character(forecastConfidence), sep='_'),
                          "high80", "low80","high95", "low95")
  output <- rbind(data.df, predictions)

  plot <- ggplot2::ggplot(output,ggplot2::aes(x=index)) + ggplot2::geom_line(ggplot2::aes(y=data),col="#a7a9ac") + ggplot2::geom_line(ggplot2::aes(y=forecasts), col="#004990")+
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
