#' Mysample Function
#'
#' @param N A quantitative vector which represents how many seats are available on the flight
#' @param gamma A quantitative vector that represents the confidence interval or the probability of over booking a flight
#' @param p A quantitative vector that represents the probability of a individual passenger showing up for the flight
#'
#' @return A list containing:
#' \item{nd}{The optimal number of tickets based on the binomial distribution (discrete).}
#' \item{nc}{The optimal number of tickets based on the approximate normal distribution (continuous).}
#' \item{gamma}{The probability of over booking the flight, which was imputed.}
#' \item{p}{The probability of an individual showing up for flighty, which was imputed.}
#' \item{N}{The number of seats on flighty, which was imputed.}
#'
#' @export
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)

ntickets <- function(N, gamma, p) {

  n_range <- seq(N, N + 20)
  nd <- max(n_range[1 - pbinom(N, size = n_range, prob = p) <= gamma])


  z_gamma <- qnorm(1 - gamma)
  n_values <- seq(N, N + 20)
  obj_normal <- 1 - gamma - pnorm(N, mean = n_values * p, sd = sqrt(n_values * p * (1 - p)))


  nc <- n_values[which.min(abs(obj_normal))]


  obj_binomial <- 1 - gamma - pbinom(N, size = n_values, prob = p)


  plot(n_values, obj_binomial, type = "o", col = "blue", pch = 19, lwd = 2, cex = 1.2,
       main = paste("Objective Vs n to find optimal tickets sold\n(", nd, ") gamma=", gamma, " N=", N, " discrete"),
       xlab = "Number of Tickets Sold", ylab = "Objective Function")
  grid()
  abline(h = 0, col = "red", lty = 2)
  abline(v = nd, col = "red", lty = 2)


  plot(n_values, obj_normal, type = "l", col = "black", lwd = 2,
       main = paste("Objective Vs n to find optimal tickets sold\n(", nc, ") gamma=", gamma, " N=", N, " continuous"),
       xlab = "Number of Tickets Sold", ylab = "Objective Function")
  grid()
  abline(h = 0, col = "blue", lty = 2)
  abline(v = nc, col = "blue", lty = 2)

  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}

