#' ADF Test for Random Walk
#'
#' Performs the Augmented Dickey-Fuller test on a given series to test for a unit root.
#'
#' @param X Numeric vector, the series of log returns or prices.
#'
#' @return A list containing the ADF statistic, p-value, and conclusion of the test.
#' @export
#'
#' @examples
#' prices <- c(100, 101, 102, 103, 102, 101)
#' adf_test(prices)

adf_test <- function(X) {
  library(tseries)
  # Ensure the input is numeric
  if (!is.numeric(X)) {
    stop("X must be a numeric vector.")
  }

  # Load necessary package
  if (!requireNamespace("tseries", quietly = TRUE)) {
    install.packages("tseries")
    library(tseries)
  }

  # Running ADF test
  test_result <- adf.test(X, alternative = "stationary")

  # Extracting values
  adf_statistic <- test_result$statistic
  p_value <- test_result$p.value

  conclusion <- ifelse(p_value < 0.05,
                       "Reject the null hypothesis of a unit root. Evidence of stationarity.",
                       "Fail to reject the null hypothesis of a unit root. No evidence of stationarity.")

  return(list(adf_statistic = adf_statistic, pvalue = p_value, conclusion = conclusion))
}
