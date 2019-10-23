library(testthat)
library(casl)
library(MASS)

context("Test the output of homework 2.")

test_that("ridge_regression() function works.", {
  # Use the iris data as an example
  data(iris)
  my_ridge <- as.numeric(ridge_regression(formula = Sepal.Length ~ .,
                                          data = iris,
                                          lambda = 0.13)$coefficients)
  # Use the output from casl_lm_ridge as the reference
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
  # Use the output from casl_lm_ridge as the reference
  X <- model.matrix(Sepal.Length ~ ., iris_c)
  y <- as.matrix(iris_c$Sepal.Length)
  casl_ridge <-  as.numeric(casl_lm_ridge(X, y, lambda = 0.08))

  expect_equivalent(my_ridge, casl_ridge, tolerance = 1e-5)
})

test_that("ridge_regression() function works, tested with lm.ridge.", {
  # Generate a data with collinear X
  set.seed(2)
  X <- matrix(rnorm(1000 * 5), ncol = 5)
  X[ ,1] <- X[ ,1] * 0.001 + X[ ,2] * (1 - 0.001)
  beta <- c(1, rep(0, 5 - 1))
  set.seed(1)
  y <- X %*% beta + rnorm(nrow(X))
  data <- data.frame(X, y)
  best_lambda(formula = y ~ ., lambda_list = 10^seq(-2, 2, by = .1),
              data = data, intercept = FALSE)
  my_ridge <- as.numeric(ridge_regression(formula = y ~ .,
                                          data = data, intercept = FALSE,
                                          lambda = 0.08)$coefficients)
  # Use lm.ridge from MASS library as the reference
  lm_ridge <- lm.ridge(y ~. -1, data, lambda = 0.08)
  MASSoutput <- unlist(lm_ridge$coef)
  expect_equivalent(my_ridge, MASSoutput, tolerance = 0.1)
})

test_that("best_lambda() function works, tested with lm.ridge.", {
  # Generate a data with collinear X
  set.seed(2)
  X <- matrix(rnorm(1000 * 5), ncol = 5)
  X[ ,1] <- X[ ,1] * 0.001 + X[ ,2] * (1 - 0.001)
  beta <- c(1, rep(0, 5 - 1))
  set.seed(1)
  y <- X %*% beta + rnorm(nrow(X))
  data <- data.frame(X, y)
  my_lambda <- best_lambda(formula = y ~ .,
                           lambda_list = 10^seq(-2, 2, by = .1),
                           data = data, intercept = FALSE)
  # Use lm.ridge from MASS library as the reference
  lm_ridge <- lm.ridge(y ~. -1, data, lambda =  10^seq(-2, 2, by = .1))
  lm_ridge_best <- as.numeric(names(which.min(lm_ridge$GCV)))

  expect_equivalent(my_lambda, lm_ridge_best, tolerance =  1e-5)
})

