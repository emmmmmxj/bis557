#' Implement a ridge regression taking collinear features into account.
#'
#' @param formula An object of class formula to describe how the model is fitted.
#' @param data A data frame.
#' @param lambda  An object of class numeric for the shrinkage parameter lambda.
#' @param intercept A logical value to indicate if intercept is included,
#' default = TRUE
#' @return A list of vectors for coefficients, lambda and formula.
#' @examples
#' data(iris)
#' ridge_regression(formula = Sepal.Length ~ ., data = iris, lambda = 1.8)
#' @export

ridge_regression <- function(formula, data, lambda, intercept = TRUE) {
  # Extract features and the label from the data
  rownames(data) <- NULL
  if (intercept == TRUE) {
    X <- model.matrix(formula, data)
  } else {
    X <- model.matrix(formula, data)[ , -1]
  }
  # Ensure missing values are removed from both features and the label
  y <- data[[as.character(formula)[2]]][as.numeric(rownames(X))]
  # Use singular value decomposition to deal with collinear features
  svd_obj <- svd(X)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  D <- diag(svals / (svals^2 + lambda))
  # Solve for ridge estimated coefficients
  ridge_beta <- as.numeric(V %*% D %*% t(U) %*% y)
  # Output results to a list
  names(ridge_beta) <- colnames(X)
  output <- list(coefficients = ridge_beta,
                 lambda = lambda,
                 formula = formula)
  return(output)
}
