#' Bootstrap Confidence Interval Estimator
#'
#' Computes and plots both the theoretical and bootstrap confidence intervals
#' for a user-defined statistic.
#'
#' @param iter Number of bootstrap iterations.
#' @param x Numeric vector of sample data.
#' @param fun A statistic function to apply (e.g., mean, median).
#' @param alpha Significance level for the confidence interval (default is 0.05).
#' @param ... Additional plotting arguments passed to hist().
#'
#' @return A named list with the sample statistic, t-value, and both the theoretical
#' and bootstrap confidence intervals.
#'
#' @importFrom stats qt quantile sd
#' @importFrom graphics axis points segments text
#' @export
#'
#' @examples
#' set.seed(35)
#' x <- round(rnorm(20, mean = 10, sd = 4), 2)
#' myboot2(10000, x, fun = "mean")
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, ...) {
  if (is.character(fun)) {
    fun <- match.fun(fun)
  }

  n <- length(x)
  stat <- fun(x)
  rsample <- matrix(sample(x, n * iter, replace = TRUE), nrow = iter)
  res_stats <- apply(rsample, 1, fun)

  ci_boot <- quantile(res_stats, c(alpha / 2, 1 - alpha / 2))
  stderr <- sd(x) / sqrt(n)
  tval <- qt(1 - alpha / 2, df = n - 1)
  ci_theoretical <- stat + c(-1, 1) * tval * stderr

  hist(res_stats, freq = FALSE, main = "Bootstrap Distribution",
       xlab = "Resampled Statistics", ...)
  abline(v = ci_boot, col = "red", lwd = 2)
  abline(v = ci_theoretical, col = "blue", lwd = 2)
  abline(v = stat, col = "black", lwd = 2)
  text(stat, 0.01, labels = round(stat, 2), pos = 3)

  return(list(
    stat = stat,
    x = x,
    tval = tval,
    ci_theoretical = ci_theoretical,
    ci_boot = ci_boot
  ))
}

