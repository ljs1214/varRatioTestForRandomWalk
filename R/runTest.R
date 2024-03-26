#' Runs Test for Random Walk
#'
#' Performs the runs test on a given series to test the random walk hypothesis.
#'
#' @param X Numeric vector, the series of log returns or prices.
#'
#' @return A list containing the z-score and p-value of the runs test.
#' @export
#'
#' @examples
#' returns <- c(100, 101, 102, 103, 102, 101)
#' runs_test(returns)

runs_test <- function(X) {
  # Ensure the input is numeric
  if (!is.numeric(X)) {
    stop("X must be a numeric vector.")
  }

  X <- diff(log(X)) # Uncomment if X is prices, not returns

  # Convert the series to binary (1 for positive, 0 for non-positive)
  signs <- as.integer(X > 0)
  runs <- sum(diff(signs) != 0) + 1
  n1 <- sum(signs == 1)
  n2 <- sum(signs == 0)
  expected_runs <- 1 + (2 * n1 * n2) / (n1 + n2)
  std_runs <- sqrt((2 * n1 * n2 * (2 * n1 * n2 - n1 - n2)) / ((n1 + n2)^2 * (n1 + n2 - 1)))

  z_score <- (runs - expected_runs) / std_runs

  p_value <- 2 * (1 - pnorm(abs(z_score)))

  conclusion <- ifelse(p_value < 0.05,
                       "Reject the null hypothesis of a random walk. Evidence of market inefficiency.",
                       "Fail to reject the null hypothesis of a random walk. No evidence of market inefficiency.")

  return(list(zscore = z_score, pvalue = p_value, conclusion = conclusion))
}
