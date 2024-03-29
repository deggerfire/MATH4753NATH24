#' nticket function for project 1
#'
#' The nticket function for project 1. Given an number of seats, the probability
#' the plane is overbook and the probability of a show, it will output the
#' number of seats that can be sold without causing pain and graphs.
#'
#' @param N Number of seats on the plane
#' @param gamma Probability of plane being overbook
#' @param p Probability of a show
#'
#' @return A list with the details the of experiment
#' @export
#'
#' @examples
#' ntickets(N=400,gamma = 0.02, p = 0.95)
ntickets <- function(N,gamma,p){
  # Import needed libraries
  library(ggplot2)    # Plots
  library(gridExtra)  # Allows for layout functionality with ggplot

  # Make vector list for the range of values to check, going 10% past N
  # A better end point could be when both D&N are equal to 1
  v <- ((N):(N * 1.1))

  ##############
  #  Discrete  #
  ##############
  # Get pain vector
  pain <- 1 - gamma - pbinom(N, v, p)
  # Get best value for discrete using which.min
  dbest <- v[which.min(abs(pain))]

  # Make the discrete plot
  g1 = ggplot(mapping = aes(x = v, y = 1 - gamma - pbinom(N, v, p)))
  g1 = g1 + geom_point(col = "blue") + geom_line()
  g1 = g1 + geom_hline(yintercept = 1 - gamma - pbinom(N, dbest, p), col = 'red')
  g1 = g1 + geom_vline(xintercept = dbest, col = 'red')
  g1 = g1 + xlab("n") + ylab("Objective")
  g1 = g1 + ggtitle(sprintf("Objective Vs n to find optimal tickets sold\n (%s) gamma=%s N=%s discrete", dbest, gamma, N))
  g1 = g1 + theme(plot.title = element_text(hjust = 0.5))

  ##############
  # Continuous #
  ##############
  # Make the function for optimize
  cont <- function(x, N, gamma, p){
    return(abs(1 - gamma - pnorm(N+.5, x * p, sd = sqrt(x*p*(1-p)))))
  }
  # Get pain vector
  pain <- 1 - gamma - pnorm(N+.5, v * p, sqrt(v*p*(1-p)))
  # Get best value for continuous using optimize
  cbest <- optimize(cont, c(N, N+20), N, gamma, p)$minimum

  # Make the continuous plot
  g2 = ggplot(mapping = aes(x = v, y = pain))
  g2 = g2 + geom_line()
  g2 = g2 + geom_hline(yintercept = 1 - gamma - pnorm(N+.5, cbest * p, sqrt(cbest*p*(1-p))), col = 'red')
  g2 = g2 + geom_vline(xintercept = cbest, col = 'red')
  g2 = g2 + xlab("n") + ylab("Objective")
  g2 = g2 + ggtitle(sprintf("Objective Vs n to find optimal tickets sold\n (%s) gamma=%s N=%s continuous", cbest, gamma, N))
  g2 = g2 + theme(plot.title = element_text(hjust = 0.5))

  # Print the graphs
  grid.arrange(g1, g2, nrow = 2)

  # Return the values
  invisible(list(nd = dbest, nc = cbest, N = N, p = p, gamma = gamma))
}
