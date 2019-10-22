library(testthat)
library(casl)

context("Test the output of homework 2.")

test_that("ridge_regression() function works.", {
  # Use the iris data as an example
  data(iris)
  my_ridge <- as.numeric(ridge_regression(formula = Sepal.Length ~ .,
                                          data = iris,
                                          lambda = 0.13)$coefficients)
  # Use the output from casl_ridge as the reference
  X <- model.matrix(Sepal.Length ~ ., iris)
  y <- as.matrix(iris$Sepal.Length)
  casl_ridge <-  as.numeric(casl_lm_ridge(X, y, lambda = 0.13))

  expect_equivalent(my_ridge, casl_ridge, tolerance = 1e-5)
})

test_that("ridge_regression() function works for collinear design matrix.", {
  data(iris)
  iris_c <- iris
  # Make Sepal.Width a linear combination of itself and Petal.Length
  iris_c[ ,2] <- iris_c[ ,2] * 0.001 + iris_c[ ,3] * (1 - 0.001)

  my_ridge <- as.numeric(ridge_regression(formula = Sepal.Length ~ .,
                                          data = iris_c,
                                          lambda = 0.08)$coefficients)

  X <- model.matrix(Sepal.Length ~ ., iris_c)
  y <- as.matrix(iris_c$Sepal.Length)
  casl_ridge <-  as.numeric(casl_lm_ridge(X, y, lambda = 0.08))

  expect_equivalent(my_ridge, casl_ridge, tolerance = 1e-5)
})
