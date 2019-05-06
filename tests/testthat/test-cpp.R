context("Check Exported C++ Functions")
library(SSOSVM)

test_that("Test Square Hinge", {
  #set up for test
  N=10^3
  DIM=sample(2:6,1)

  YMAT <- generateSim(N, 2, DIM, NULL)
  EPSILON = 0.00001

  returnAll=TRUE
  rho=1.0

  result<-SquareHinge(YMAT$YMAT, DIM, returnAll, rho)

  #types correct
  expect_is(result, "list")
  expect_is(result[[1]], "numeric")
  expect_is(result[[2]], "integer")
  expect_is(result[[3]], "integer")
  expect_is(result[[4]], "matrix")
  expect_is(result[[5]], "numeric")

  #dims
  expect_equal(result[[2]], N)
  expect_equal(result[[3]], DIM)
  expect_equal(length(result[[1]]), DIM+1)
  expect_equal(dim(result[[4]]), c(N,DIM+1))

  #no missing
  expect_identical(result, na.omit(result))

})

test_that("Test Hinge", {
  #set up for test
  N=10^3
  DIM=sample(2:6,1)

  YMAT <- generateSim(N, 2, DIM, NULL)
  EPSILON = 0.00001

  returnAll=TRUE
  rho=1.0

  result<-Hinge(YMAT$YMAT, DIM, returnAll, rho)

  #types correct
  expect_is(result, "list")
  expect_is(result[[1]], "numeric")
  expect_is(result[[2]], "integer")
  expect_is(result[[3]], "integer")
  expect_is(result[[4]], "matrix")
  expect_is(result[[5]], "numeric")

  #dims
  expect_equal(result[[2]], N)
  expect_equal(result[[3]], DIM)
  expect_equal(length(result[[1]]), DIM+1)
  expect_equal(dim(result[[4]]), c(N,DIM+1))

  #no missing
  expect_identical(result, na.omit(result))

})

test_that("Test Logistic", {
  #set up for test
  N=10^3
  DIM=sample(2:6,1)

  YMAT <- generateSim(N, 2, DIM, NULL)
  EPSILON = 0.00001

  returnAll=TRUE
  rho=1.0

  result<-Logistic(YMAT$YMAT, DIM, returnAll, rho)

  #types correct
  expect_is(result, "list")
  expect_is(result[[1]], "numeric")
  expect_is(result[[2]], "integer")
  expect_is(result[[3]], "integer")
  expect_is(result[[4]], "matrix")
  expect_is(result[[5]], "numeric")

  #dims
  expect_equal(result[[2]], N)
  expect_equal(result[[3]], DIM)
  expect_equal(length(result[[1]]), DIM+1)
  expect_equal(dim(result[[4]]), c(N,DIM+1))

  #no missing
  expect_identical(result, na.omit(result))

})
