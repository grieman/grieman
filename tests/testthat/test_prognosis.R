library(grieman)
context("Prognosis")

test_that("arma models are generated and written", {
  series <- GenerateSeriesToCutoff(100, .01, .005, 200, 100)
  expect_lt(length(series), 100)

  Prognosis_output <- Prognosis_ARIMA(series)

  prognosis_string <- modelString(Prognosis_output$Model)
  expect_equal(class(prognosis_string),'character')
})
