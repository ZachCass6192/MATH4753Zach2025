#' Central Limit Theorem Simulation for Uniform Distribution
#'
#' Simulates the CLT using uniform(a, b) distributions. Repeatedly samples and sums random values.
#'
#' @param n Sample size for each iteration
#' @param iter Number of iterations/samples
#' @param a Lower bound of uniform distribution (default = 0)
#' @param b Upper bound of uniform distribution (default = 5)
#'
#' @return A histogram with a normal distribution overlay; returns the vector of simulated sums
#' @importFrom grDevices rainbow
#' @importFrom stats dnorm
#' @importFrom graphics hist runif curve
#' @export
myclt <- function(n, iter, a = 0, b = 5) {
  y <- runif(n * iter, a, b)                       # Generate uniform data
  data <- matrix(y, nrow = n, ncol = iter)         # Reshape into a matrix
  sm <- apply(data, 2, sum)                        # Sum each sample (column)

  hist(sm,
       col = rainbow(length(sm)),
       freq = FALSE,
       main = "Distribution of the Sum of Uniforms",
       xlab = "Sum of sampled values")

  # Calculate theoretical mean and sd for sum of uniforms
  mu <- n * (a + b) / 2
  sigma <- sqrt(n * (b - a)^2 / 12)

  # Overlay normal curve
  curve(dnorm(x, mean = mu, sd = sigma),
        from = min(sm), to = max(sm), add = TRUE, lwd = 2, col = "blue")

  return(sm)
}

