#' armaString
#'
#' This function outputs a string of the arma model's equation
#'
#' @param model arma/arima model to write as a string
#' @param round_to number of decimal places to keep
#'
#' @return a string
#' @export
#'
#' @examples
#' series <- c(1, 3, 2, 3, 4, 3, 4, 5, 6, 4, 5, 6, 5, 4, 7, 5, 4, 7, 8, 5, 7, 8, 9, 7, 9)
#' arimamodel <- forecast::auto.arima(series)
#' armaString(arimamodel)
#'
armaString <- function(model, round_to = 2) {
  arimap <- model$arma[1]; arimad <- model$arma[6]; arimaq <- model$arma[2]

  str_p <- c(); str_q <- c(); p_string <- ""; q_string <- ""
  if(arimap > 0){
    for(i in 1:arimap){
      str_p[i] <- sprintf("+ %s y_{t-%s} ", signif(model$coef[i],round_to),(i))
      p_string = paste(p_string,str_p[i], sep="")
    }
  }
  if(arimaq > 0){
    for(i in 1:arimaq){
      str_q[i] <- sprintf("+ %s \\varepsilon_{t-%s}", signif(model$coef[i+arimap],round_to),(i))
      q_string = paste(q_string, str_q[i], sep="")
    }
  }
  arma_string = paste(p_string, q_string, sep="")
  sprintf("$$p_t = \\varepsilon_t%s$$",arma_string)
}
