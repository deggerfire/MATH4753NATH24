#' myncurve function
#'
#' Plots a normal distribution curve that shows how much
#' and where a lower tail prop is
#'
#' @param mu Mean of the function
#' @param sigma Standard deviation of the function
#' @param a Right endpoint
#'
#' @export
#'
#' @examples
#' myncurve()
myncurve = function(mu = 0, sigma = 1, a = .5){
  pnorm <- NULL
  curve <- NULL
  dnorm <- NULL
  x <- NULL
  polygon <- NULL

  prob = pnorm(a, mean = mu, sd = sigma)
  prob = round(prob,4)

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), xlab = paste("Area = ", prob, sep=""))

  xcurve = seq(-1000, a, length = 100000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(-1000, xcurve, a), c(0, ycurve, 0), col="Red")

  list(mu = mu, sigma = sigma, prob = prob)
}
