library(grieman)
context("TimeSeriesAnalysis")

test_that("analysis outputs correctly",{
  data <- c(rnorm(50,0,1), rnorm(50,0,2),20,rnorm(50,10,1))
  dates <- seq(as.Date("2016/1/1"), by="days", length.out=151)
  result <- TimeSeriesAnalysis(data,dates)
  expect_equal(class(result),'data.frame')
})

test_that("plotly works",{
  data <- c(rnorm(50,0,1), rnorm(50,0,2),20,rnorm(50,10,1))
  dates <- seq(as.Date("2016/1/1"), by="days", length.out=151)
  result <- TimeSeriesAnalysis(data,dates, plotly=TRUE)
  expect_equal(class(result),'data.frame')
})
