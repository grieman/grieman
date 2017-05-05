library(grieman)
context("Unsupervised")

test_that("dendogram works", {
  set.seed(10)
  datain <- data.frame(matrix(rnorm(400), nrow=100))

  dataout <- UnsupervisedClusters(datain, plottype = "dendogram")

  expect_equal(class(dataout),'data.frame')
})

test_that("other plots work", {
  set.seed(10)
  datain <- data.frame(matrix(rnorm(400), nrow=100))

  dataout <- UnsupervisedClusters(datain, plottype = "fan")

  expect_equal(class(dataout),'data.frame')
})


test_that("oconfusion matrix prints", {
  set.seed(10)
  datain <- data.frame(matrix(rnorm(400), nrow=100))

  dataout <- UnsupervisedClusters(datain, plottype = "fan", classification =  rep(c(0,1),50))

  expect_equal(class(dataout),'data.frame')
})
