#' ggpairs plot, with group column
#'
#' @param data data frame
#' @param filtercolumn column name of data to color by
#' @param cutoff value or values at which to split the filtercolumn. Integer or vector
#'
#' @return A ggpairs plot
#' @export
#'
#' @examples
#' set.seed(1)
#' datain <- data.frame(matrix(rnorm(100), nrow=25))
#' colnames(datain) <- c("w","x","y","z")
#'
#' ThresholdPairs(datain, "x", cutoff=c(-.33,.33))
#'
ThresholdPairs <- function(data, filtercolumn, cutoff) {
  colrcol <- data[[filtercolumn]]
  colr <- rep(0, length(colrcol))

  #Seperate into groups
  for (i in 1:length(cutoff)) {
    colr[which(colrcol > cutoff[i])] <- i
  }

  data[[filtercolumn]] <- as.factor(colr)

  pairsplot <-
    GGally::ggpairs(
      data,
      mapping = ggplot2::aes_string(color = filtercolumn),
      columns = colnames(data[-c(which(colnames(data) == filtercolumn))])
    )

  return(pairsplot)
}
