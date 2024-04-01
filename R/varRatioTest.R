#' Variance Ratio Test
#'
#' Performs the variance ratio test on a given series of log prices to test the random walk hypothesis.
#'
#' @param S Numeric vector, the prices series.
#' @param q Integer, the lag number.
#' @param types String, choose from [homo, hetero]
#'
#' @return A list containing the z-score and p-value of the variance ratio test.
#' @export
#'
#' @examples
#' log_prices <- log(c(100, 101, 102, 103, 102, 101))
#' variance_ratio_test(prices, 2)
variance_ratio_test <- function(S, q, types) {

  # error check
  if (!is.numeric(S)) {
    stop("S must be a numeric vector.")
  }
  if (q <= 0) {
    stop("q must be a positive integer.")
  }
  if (length(S) < 2*q) {
    stop("The length of S must be at least twice q.")
  }
  if (!is.character(types) || !(types %in% c("homo", "hetero"))) {
    stop("types must be 'homo' or 'hetero'.")
  }

# main feature
# homo
  S <- log(S)
  n <- floor((length(S) - 1) / q)
  muhat <- (S[n*q + 1] - S[1]) / n / q
  if (types == 'homo') {
  m <- q * (n*q - q + 1) * (1 - q / (n*q))
  VC <- sum((S[seq(1 + q, (n*q + 1))] - S[seq(1, (n*q - q + 1))] - q*muhat)^2) / m
  VA <- sum((S[seq(2, (n*q + 1))] - S[seq(1, n*q)] - muhat)^2) / (n*q - 1)
  phiq <- 2 * (2*q - 1) * (q - 1) / (3 * q * n * q)
  zscore <- (VC / VA - 1) / sqrt(phiq)
  pvalue <- (1 - pnorm(abs(zscore))) * 2
  } else{
# hetero
    if (types == 'hetero') {
      delta <- numeric(q)
      for(j in 1:q) {
        numerator <- sum((S[seq(j, length(S)-1, q)] - S[seq(j-1, length(S)-q-1, q)] - muhat)^2)
        denominator <- numerator + sum((S[seq(j+q, length(S), q)] - S[seq(j, length(S)-q-1, q)] - muhat)^2)
        delta[j] <- numerator / denominator
      }
      phi_star_q <- sum((2 * (1:q) - 1)^2 * (delta / q^2))
      m <- q * (n*q - q + 1) * (1 - q / (n*q))
      VC <- sum((S[seq(1 + q, (n*q + 1))] - S[seq(1, (n*q - q + 1))] - q*muhat)^2) / m
      VA <- sum((S[seq(2, (n*q + 1))] - S[seq(1, n*q)] - muhat)^2) / (n*q - 1)
      zscore <- (VC / VA - 1) / sqrt(phi_star_q)
      pvalue <- (1 - pnorm(abs(zscore))) * 2
    }
  }
  conclusion <- ifelse(pvalue < 0.05,
                       "Reject the null hypothesis of a random walk. Evidence of market inefficiency.",
                       "Fail to reject the null hypothesis of a random walk. No evidence of market inefficiency.")

  return(list(zscore = zscore, pvalue = pvalue, conclusion = conclusion))
}
