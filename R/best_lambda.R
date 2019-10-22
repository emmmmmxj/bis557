#' Optimize the ridge parameter lambda with k-fold cross-validation
#'
#' @param formula An object of class formula to describe how the model is fitted
#' @param data A data frame
#' @param nfold An optional number k for k-fold cross-validation, default = 5
#' @param lambda_list  A numeric vector consisting of lambda candidates
#' @param intercept An optional logical value to indicate if intercept is
#' included, default = TRUE
#' @return Return the best lambda value
#' @examples
#' data(iris)
#' best_lambda(formula = Sepal.Length ~ .,
#'             data = iris, nfold = 5,
#'             lambda_list = 10^seq(-2, 2, by = .1))
#'
#' data(mtcars)
#' best_lambda(formula = mpg ~ drat + wt + qsec,
#'             data = mtcars, nfold = 3,
#'             lambda_list = 10^seq(-2, 2, by = .1))
#' @export

best_lambda <- function(formula, data, nfold = 5, lambda_list,
                        intercept = TRUE) {
  # Calculate number of observations in each fold (fold size)
  fsize <- floor(nrow(data) / nfold)
  # Initialize a list for cross-validation MSE
  cv_mse <- rep(NA, length(lambda_list))
  # Loop through each lambda candidate
  for (i in seq_along(lambda_list)) {
    lambda <- lambda_list[i]
    val_mse <- rep(NA, nfold)
    # Create start and end indices to subset folds
    if (nrow(data) %% nfold == 0) {
      start_ind <- seq(from = 1, to = nrow(data), by = fsize )
      end_ind <- seq(from = fsize, to = nrow(data), by = fsize)
      # Ensure that the function can divide dataset into folds even when the
      # number of observations is not divisible by the number of folds
    } else if (nrow(data) %% nfold != 0) {
      start_ind <- seq(from = 1, to = nrow(data), by = fsize )[1:nfold]
      end_ind <- seq(from = fsize, to = nrow(data), by = fsize)
      end_ind[nfold] <- nrow(data)
    }
    # For each lambda candidate, loop through each fold
    for (k in 1:nfold) {
      rownames(data) <- NULL
      # Create a validation set for each k
      val <- data[start_ind[k]:end_ind[k], ]
      # Make whether to include the intercept an option
      # Separate features and the label for the validation set
      if (intercept == TRUE) {
        val_X <- model.matrix(formula, val)
      } else {
        val_X <- model.matrix(formula, val)[ , -1]
      }
      val_y <- val[[as.character(formula)[2]]]
      # Create the training set
      train <- data[setdiff(as.numeric(rownames(data)),
                            as.numeric(rownames(val_X))), ]
      # Calculate and extract ridge beta coefficients based on the training set
      train_coef <- as.matrix(ridge_regression(formula, train, lambda,
                                            intercept = intercept)$coefficients)
      # Use ridge coefficients obtained from the training set to predict the
      # label for the validation set
      y_pred <- val_X %*% train_coef
      # Calculate validation MSE
      val_mse[k] <- apply((y_pred - val_y)^2, 2, mean)
    }
    # Calculate cross-validation MSE for each lambda candidate
    cv_mse[i] <- mean(val_mse)
  }
  # Return the best lambda with the smallest cross-validation MSE
  return(lambda_list[cv_mse == min(cv_mse)])
}
