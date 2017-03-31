#' ArmaToGarch
#'
#' This function takes an arima model, checks for arch effect, and fits a garch model if arch effects are present
#'
#' @param arimamodel The arima model to check for arch effects
#' @param max_arch_terms Maximum number of ARCH terms. A higher value will allow for more accuracy, but slower operation
#'
#' @return A garch model fitted on the residuals of the input arima model
#' @export
#'
#' @examples
#' series1 <- c(1, 3, 2, 3, 4, 3, 4, 5, 6, 4, 5, 6, 5, 4, 7, 5, 4, 7, 8, 5, 7, 8, 9, 7, 9)
#' arimamodel <- forecast::auto.arima(series1)
#' ArmaToGarch(arimamodel)
#'
#' series2 <- c(1, 3, 4, 1, 4, 1, 2, 5, 1, 4, 5, 6, 1, 4, 7, 5, 1, 7, 8, 5, 1, 8, 9, 7, 1)
#' arimamodel <- forecast::auto.arima(series2)
#' ArmaToGarch(arimamodel)
#'
ArmaToGarch <- function (arimamodel, max_arch_terms = 15) {
  if (class(arimamodel)[1] != "Arima" && class(arimamodel)[1] != "ARIMA"){stop("Not a valid ARIMA model")}

  if (stats::Box.test((arimamodel$residuals) ^ 2, lag = 12, type = "Ljung-Box")$p.value < .1) {
    arch_aics <- c()
    for (j in 1:max_arch_terms) {
      arch_aics[j] <- suppressWarnings(stats::AIC(tseries::garch(arimamodel$residuals[!is.na(arimamodel$residuals)], order = c(1, j), trace = F)))
    }
    best_arch <- which.min(arch_aics)
    output <- tseries::garch(arimamodel$residuals[!is.na(arimamodel$residuals)], order = c(1, best_arch),trace = F) #fits best garch model
  } else{
    output <- NA
    warning("No ARCH effect detected, therefore no GARCH model was fitted")
  }
  return(output)
}
