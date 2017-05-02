#' Models to LaTeX Formula
#'
#' Writes a model's equation in LaTeX. Supported classes are lm, arima, and garch
#'
#' @param model model to write out
#' @param round_to integer: number of significant digits to round to
#' @param dependent string: the dependent variable. Defaults to y
#'
#' @return A LaTeX formatted string
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ hp + wt, data = mtcars)
#' modelString(fit, dependent = "mpg")
#'
#' series <- c(1, 3, 2, 3, 4, 3, 4, 5, 6, 4, 5, 6, 5, 4, 7, 5, 4, 7, 8, 5, 7, 8, 9, 7, 9)
#' arimamodel <- forecast::auto.arima(series)
#' modelString(arimamodel)
#'
modelString <- function(model,
                        round_to = 2,
                        dependent  = "y") {
  if (class(model)[1] %nin% c("lm", "ARIMA", "Arima", "garch")) {
    stop("Model format not supported. Supported formats: 'lm', 'arima', and 'garch'")
  }

  if (class(model)[1] == "lm") {
    # detect intercept term and number of variables
    if (names(model$coefficients)[1] == "(Intercept)") {
      intercept = T
    } else {
      intercept = F
    }
    numterms = length(names(model$coefficients))

    # make LaTeX for coefficients
    funcstring <- c()
    for (i in (1 + as.numeric(intercept)):numterms) {
      str_i <-
        sprintf(" + %s %s",
                signif(model$coefficients[i], round_to),
                names(model$coefficients[i]))
      funcstring <- paste(funcstring, str_i, sep = "")
    }

    # add intercept term if needed
    if (intercept == T) {
      stringout <- sprintf(
        "$$\\hat{%s} = %s%s$$",
        dependent,
        signif(model$coefficients[1], round_to),
        funcstring
      )
    } else {
      stringout <- sprintf("$$\\hat{%s} =%s$$", dependent, funcstring)
    }
  } else if (class(model)[1] %in% c("ARIMA", "Arima")) {
    arimap <- model$arma[1]
    arimaq <- model$arma[2]

    #make p-string
    str_p <- c()
    str_q <- c()
    p_string <- ""
    q_string <- ""
    if (arimap > 0) {
      for (i in 1:arimap) {
        str_p[i] <-
          sprintf("+ %s y_{t-%s} ", signif(model$coef[i], round_to), (i))
        p_string <- paste(p_string, str_p[i], sep = "")
      }
    }

    #make q-string
    if (arimaq > 0) {
      for (i in 1:arimaq) {
        str_q[i] <-
          sprintf("+ %s \\varepsilon_{t-%s}",
                  signif(model$coef[i + arimap], round_to),
                  (i))
        q_string <- paste(q_string, str_q[i], sep = "")
      }
    }

    #combine
    arma_string <- paste(p_string, q_string, sep = "")
    stringout <- sprintf("$$p_t = \\varepsilon_t%s$$", arma_string)
  } else if (class(model)[1] == "garch") {
    header <- "$$ p_t = \\mu + \\sigma_t \\varepsilon_t \\\\ "
    equation_start <- sprintf("\\sigma_t^2 = %s",
                              signif(model$coef["a0"], round_to))

    str_p <- c()
    str_q <- c()
    p_string <- ""
    q_string <- ""
    for (i in 1:model$order[1]) {
      str_p[i] <-
        sprintf(" + %s \\varepsilon_{t-%s}^2",
                signif(model$coef[i + 1], round_to),
                i)
      p_string <- paste(p_string, str_p[i], sep = "")
    }
    for (i in 1:model$order[2]) {
      str_q[i] <-
        sprintf(" + %s \\sigma_{t-%s}^2", signif(model$coef[i + 1 + model$order[1]], round_to), i)
      q_string <- paste(q_string, str_q[i], sep = "")
    }
    stringout <-
      paste(header, equation_start, p_string, q_string, "$$")
  }
  return(stringout)


}
