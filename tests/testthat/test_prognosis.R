library(grieman)
context("Prognosis")

test_that("arma models are generated and written", {
  set.seed(100)
  series <- GenerateSeriesToCutoff(100, .01, .005, 200, 100)
  expect_lt(length(series), 100)

  Prognosis_output <- Prognosis_ARIMA(series)

  prognosis_string <- modelString(Prognosis_output$Model)
  expect_equal(class(prognosis_string),'character')

})

test_that("garch models are generated and written",{
  Prognosis_output <- Prognosis_ARIMA(BJsales, detailed = TRUE)

  prognosis_string <- modelString(Prognosis_output$Model)
  expect_equal(class(prognosis_string),'character')
})

test_that("seasonal works",{
  exampledata <- GenerateSeriesToCutoff(100, .01, .005, 200, 100)
  times <- seq(as.Date("2000/1/1"), by = "month", length.out = length(exampledata))
  Prognosis_output<- Prognosis_Seasonal(exampledata, times, 12, detailed = TRUE)

  expect_equal(class(Prognosis_output$Model), "stlm")
})

test_that("bad inputs are caught", {
  expect_error(Prognosis_ARIMA(CO2), "Invalid data format*")
})
