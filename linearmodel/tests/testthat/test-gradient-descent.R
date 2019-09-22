library(testthat)

context("Test the output of my gradient_descent function for homework 1.")

test_that("My gradient_descent() function works", {

  data(iris)

  fit_gd <- gradient_descent(Sepal.Length ~ ., iris,
                             lambda = 0.001, iter = 3000000)

  fit_lm <- lm(Sepal.Length  ~ ., iris)

  expect_equivalent(fit_lm$coefficients, fit_gd$coefficients,
                    tolerance = 1e-5)
})

