#' GenerateSeriesToCutoff
#'
#'This function generates a random series with a set trend, and will cut the series as soon as it crosses a certain threshold. The series, p, is generated as follows: p[i - 1] * exp(rnorm(1, mu, sigma)
#'
#' @param N Maximum length of series
#' @param mu mean
#' @param sigma variance
#' @param cutoff_point threshold at which to cut series
#' @param starting_point first point in generated series
#'
#' @return A vector of points
#' @export
#'
#' @examples
#' GenerateSeriesToCutoff(100, .01, .005, 200, 100)
#' GenerateSeriesToCutoff(100, .1, .001, 100, 50)
#'
GenerateSeriesToCutoff <-
  function(N, mu, sigma, cutoff_point, starting_point) {
    p <- c(0)
    while (max(p) < cutoff_point) {
      p  <- c(starting_point, rep(NA, N - 1))
      for (i in 2:N)
        p[i] <- p[i - 1] * exp(stats::rnorm(1, mu, sigma))
    } #Generates data that hits cutoff_point
    p_last <- 0
    last_point <- 0
    while (p_last < cutoff_point) {
      last_point = last_point + 1
      p_last <- p[last_point]
    } #Finds first point over cutoff_point
    generated_data <-
      p[1:(last_point)] #cuts data after first point above cutoff_point
    return(generated_data)
  }
