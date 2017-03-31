#' printClassificationTable
#'
#' @param observed Vector of actual values
#' @param predicted Vector of predicted values
#' @param method A string to pass into the output, identifies the model used.
#'
#' @return Prints a classification Matrix and calculates the misclassification rate.
#' @export
#'
#' @examples
#' printClassificationTable(c(1,2,1,2,1), c(1,2,2,1,1), method="input")
printClassificationTable <- function(observed, predicted, method){
  classDF <- data.frame(observed = observed, predicted = predicted)
  tabl<- stats::xtabs(~ predicted + observed, data = classDF)
  missrate <- 100*(1-sum(diag(tabl)) / sum(tabl))
  tabl<- stats::addmargins(tabl)
  print(tabl)
  print(sprintf('This %s misclassifies %.2f %% of the test data',method, missrate))
}
