#' modelString
#'
#' Writes a model's equation in LaTeX
#'
#' @param model model to write out
#' @param round_to integer: number of significant digits to round to
#' @param dependent string: the dependent variable. Defaults to y
#'
#' @return a latex formatted string
#' @export
#'
#' @examples
#' model <- lm(mpg~.,data=mtcars)
#' modelString(model)
#'
modelString <- function(model, round_to=2, dependent = "y"){
  if(names(model$coefficients)[1] == "(Intercept)"){intercept=T} else {intercept = F}
  numterms = length(names(model$coefficients))

  funcstring <- c()
  for (i in (1+as.numeric(intercept)):numterms){
    str_i <- sprintf(" + %s \text{%s}", signif(model$coefficients[i], round_to), names(model$coefficients[i]))
    funcstring <- paste(funcstring, str_i, sep="")
  }

  if (intercept == T){
    sprintf("$$\\hat{%s} = %s%s$$", dependent, signif(model$coefficients[1], round_to), funcstring)
  } else {
    sprintf("$$\\hat{%s} = %s$$", dependent, funcstring)
  }

  }
