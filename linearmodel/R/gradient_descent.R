#' Implement gradient descent for ordinary least squares.
#'
#' @param formula An object of class formula to describe how the model is fitted.
#' @param data A data frame.
#' @param contrasts  An optional list of contrasts for factor variables.
#' @param lambda A number for the learning rate, default = 0.0001.
#' @param iter  A number for the number of iterations, default = 10000
#' @return A list of vectors for coefficients, residuals and fitted.values.
#' @examples
#' data(iris)
#' gradient_descent(Sepal.Length ~ ., iris, lambda = 0.001, iter = 3000000)
#' @export

gradient_descent <- function(formula, data, contrasts = NULL,
                             lambda = 0.0001, iter = 10000) {
  # Create the design matrix X
  X <- model.matrix(object = formula, data = data, contrasts = contrasts)
  # Create the response variable Y
  vars <- all.vars(formula)
  Y <- data[ ,vars[1]]

   # Initialize a vector Ls to hold values of the loss function
  Ls <- rep(0, iter)
  # Initialize a list betas to hold values of updated beta
  betas <- list(iter)
  # Initialize the beta matrix
  beta <- matrix(rep(1, ncol(X)), nrow = ncol(X))

  # Iterate gradient descent
  for (i in 1:iter) {
    dL <- (2 * t(X) %*% X %*% beta - 2 * t(X) %*% Y) / length(Y)
    beta <- beta - lambda * dL
    betas[[i]] <- beta
    Ls[i] <- (t(Y - X %*% beta) %*% (Y - X %*% beta)) / length(Y)
  }

  # Find optimal beta that minimizes the loss function
  opt_beta <- betas[[which.min(Ls)]]

  # Return a list of vectors
  results <- list(coefficients = as.numeric(opt_beta),
                  residuals = as.numeric(Y - X %*% opt_beta),
                  fitted.values = as.numeric(X %*% opt_beta))
  # Assign variable names to the coefficient vector
  names(results$coefficients) <- colnames(X)

  return(results)
}

