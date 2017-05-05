library(grieman)
context("Prognosis")

test_that("bad input is caught", {
  expect_error(ArmaToGarch(lm(mpg~wt, data=mtcars)), "Not a valid*")
})

test_that("transformation works", {
  series2 <- c(1, 3, 4, 1, 4, 1, 2, 5, 1, 4, 5, 8, 1, 4, 7, 5, 1, 7, 8, 5, 1, 8, 9, 7, 1)
  arimamodel <- forecast::auto.arima(series2)
  newmodel <- ArmaToGarch(arimamodel, max_arch_terms = 5)
  expect_equal(class(newmodel),'garch')
})

test_that("arima stays arima", {
  series1 <- c(1, 3, 2, 3, 4, 3, 4, 5, 6, 4, 5, 6, 5, 4, 7, 5, 4, 7, 8, 5, 7, 8, 9, 7, 9)
  arimamodel <- forecast::auto.arima(series1)
  newmodel <- ArmaToGarch(arimamodel, max_arch_terms = 5)
  expect_equal(class(newmodel),'logical')
})
