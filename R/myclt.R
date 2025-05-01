#' Central Limit Theorem Simulation
#'
#' This function simulates the sum of uniformly distributed random variables
#' and visualizes the distribution of the sums. It uses the Central Limit Theorem
#' to show how the distribution of sums approaches a normal distribution as
#' the number of samples increases.
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve grid hist
#' @importFrom stats dnorm pbinom pnorm qnorm runif
#'
#'
#'
#' @param n Number of samples in each iteration. Default is 100.
#' @param iter Number of iterations to simulate. Default is 1000.
#' @param a Lower bound of the uniform distribution. Default is 0.
#' @param b Upper bound of the uniform distribution. Default is 5.
#'
#' @return A vector of the sums from each iteration.
#'
#'
#' @export
myclt <- function(n, iter, a = 0, b = 5)
  {
  y = runif(n * iter, a, b)  # Uniform random numbers
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE)  # Reshape into matrix
  sm = apply(data, 2, sum)  # Sum over rows
  h = hist(sm, plot = FALSE)  # Get histogram data without plotting
  hist(sm, col = rainbow(length(h$mids)), freq = FALSE,
       main = "Distribution of the sum of uniforms")

  # Plot normal distribution on top
  curve(function(x) dnorm(x, mean = n * (a + b) / 2, sd = sqrt(n * (b - a)^2 / 12)),
        from = min(sm), to = max(sm), add = TRUE, lwd = 2, col = "Blue")

  sm  # Return the simulated sums
}

