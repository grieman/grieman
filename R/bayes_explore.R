#' bayes_explore
#'
#' This function uses stan to allow the user to make a bayesian model from a frequentist one, allowing for easy bayesian conclusions (intervals, estimates, etc.)
#' There is an option to open a shiny-based analysis portal, or to output some plots with posterior distributions and population estimates.
#' The model itself is also output.
#'
#' @param model a model, in lm or glm format
#' @param shiny boolean, True opens shinystan interface while False outputs a handful of plots
#' @param show_intercept boolean. Toggles if the intercept is included in plots
#'
#' @return a model and either a shiny interface, or handful of plots
#' @export
#'
#' @examples
#' bayes_explore(lm(mpg~., mtcars))
#' bayes_explore(lm(stack.loss~., stackloss), show_intercept=FALSE)
#' bayes_explore(lm(Fertility~0+., swiss))
#' bayes_explore(glm(am~0+mpg+wt, mtcars, family=binomial(link='logit')))
#'
bayes_explore <- function(model, shiny=F, show_intercept=T){
  if(class(model)[1] == "lm"){
    model <- stats::glm(stats::formula(model), model$model, family=stats::gaussian())
  }

  bayes <- rstanarm::stan_glm(stats::formula(model),
                              data= model$model,
                              family = stats::family(model))

  if(shiny==FALSE){
    posterior <- as.array(bayes)

    ifelse(show_intercept==T, pars <- names(model$coefficients), pars <- names(model$coefficients)[names(model$coefficients) %nin% '(Intercept)'])
    print(bayesplot::mcmc_areas(posterior,
                        pars = pars,
                        prob = .8,
                        prob_outer = .95,
                        point_est = "none") + ggplot2::ggtitle("Posterior distributions",
                                                    "with 80% intervals and 2.5% missing tails"))
    print(bayesplot::ppc_dens_overlay(y = bayes$y,
                     yrep = rstanarm::posterior_predict(bayes, draws = 50)) + ggplot2::ggtitle("Estimated Population Curve",
                                                                                               "plotted over 50 estimates"))
  } else {
    shinystan::launch_shinystan(bayes)
  }
  return(bayes)
}
