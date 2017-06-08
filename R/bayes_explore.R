#' bayes_explore
#'
#' This function uses stan to allow the user to make a bayesian model from a frequentist one, allowing for easy bayesian conclusions (intervals, estimates, etc.).
#' It will return the bayesian model, and output a couple plots - an estimate of the population's y-value distribution, and confidence intervals for the model parameters.
#' Be aware of shinystan::launch_shinystan for a more interactive environment - use bayes_explore \%>\% shinystan::launch_shinystan
#'
#' @param model a model, in lm or glm format
#' @param show_intercept boolean. Toggles if the intercept is included in plots
#' @param extra_plots boolean. Toggles trace and hex plots, if applicable (hex plot needs only two parameters)
#' @param chains number of markov chains to calculate. Lower numbers trade accuracy for speed
#' @param chain_length length of calculated markov chains. Lower numbers trade accuracy for speed
#'
#' @return a model and a series of plots
#' \describe{
#'  \item{Estimated Population Curve}{This plot shows the estimated values of the dependent (y) variable in the population at large}
#'  \item{Posterior Distributions}{This shows confidence intervals for each of the model's coefficients.
#'  These distributions can be used to make proper bayesian predictions - e.g. we are 80\% confident that the true value of \eqn{\beta_i} is in this interval - as opposed to frequentist ones which use the calculated value and uncertainty to a null hypothesis' probabilty of error.}
#' }
#'
#' @examples
#' bayes_explore(lm(mpg~., mtcars))
#' bayes_explore(lm(stack.loss~., stackloss), show_intercept=FALSE)
#' bayes_explore(lm(Fertility~0+., swiss))
#' bayes_explore(glm(am~0+mpg+wt, mtcars, family=binomial(link='logit')), extra_plots=TRUE)
#'
#' @export
#'
bayes_explore <- function(model, show_intercept=T, extra_plots=F, chains = 4, chain_length = 1000){

  if(class(model)[1] == "lm"){
    model <- stats::glm(stats::formula(model), model$model, family=stats::gaussian())
  }

  bayes <- rstanarm::stan_glm(stats::formula(model),
                              data= model$model,
                              family = stats::family(model),
                              chains = chains, iter = chain_length)

  posterior <- as.array(bayes)

  ifelse(show_intercept==T, pars <- names(model$coefficients), pars <- names(model$coefficients)[names(model$coefficients) %nin% '(Intercept)'])

  print(bayesplot::ppc_dens_overlay(y = bayes$y,
                   yrep = rstanarm::posterior_predict(bayes, draws = 50)) + ggplot2::ggtitle("Estimated Population Curves",
                                                                                               "Observed values plotted atop 50 estimates"))

  Sys.sleep(4)

  if (extra_plots==T){
    print(bayesplot::mcmc_trace(posterior, pars, facet_args = list(ncol = 1, strip.position = "left")) + ggplot2::ggtitle("Markov Chain Values"))
    Sys.sleep(4)
    if(length(pars) == 2){
      print(bayesplot::mcmc_hex(posterior, pars) + ggplot2::ggtitle("Posterior Density with Two Parameters"))
      Sys.sleep(4)
    }
  }

  print(bayesplot::mcmc_areas(posterior,
                      pars = pars,
                      prob = .8,
                      prob_outer = .95,
                      point_est = "none") + ggplot2::ggtitle("Posterior Distributions",
                                                    "with 80% intervals and 2.5% missing tails"))

  #print("These plots are explained in the help file - the first is an estimate of the population's y values, and the second is the distributions of the model's Beta values.l")
  return(bayes)
}
