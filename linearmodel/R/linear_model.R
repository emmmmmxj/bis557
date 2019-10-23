#' Fit a linear model.
#'
#' @param formula An object of class formula to describe how the model is fitted.
#' @param data A data frame.
#' @param contrasts  An optional list of contrasts for factor variables.
#' @return A list of vectors for coefficients, residuals and fitted.values.
#' @examples
#' data(iris)
#' linear_model(Sepal.Length ~ ., iris)
#' @export

linear_model <- function(formula, data, contrasts = NULL) {
  # Create the design matrix X
  X <- model.matrix(object = formula, data = data, contrasts = contrasts)
  # Create the response variable Y
  vars <- all.vars(formula)
  Y <- data[ ,vars[1]]
  # Calculate estimated coefficients beta using QR decomposition
  qr_X = qr(X)
  beta = solve.qr(qr_X, Y)
  beta[beta == 0] <- NA
  # Return a list of vectors
  results <- list(coefficients = as.numeric(beta),
                  residuals = as.numeric(Y - X %*% beta),
                  fitted.values = as.numeric(X %*% beta))
  # Assign variable names to the coefficient vector
  names(results$coefficients) <- colnames(X)

  return(results)
}
