#' Pareto graphing funcation
#'
#' @param x the data in vector form
#' @param mn graph title
#' @param ... extra stuff
#'
#' @export
#'
#' @examples
#'freq=c(15,8,63,20)
#'RL=c("None","Both","LegsO","WheelsO")
#'l=rep(RL,freq)
#'pareto(l)
pareto <- function(x, mn = "Pareto barplot", ...) {
  barplot <- NULL
  axis <- NULL
  segments <- NULL
  title <- NULL
  x.tab = table(x)
  xx.tab = sort(x.tab, decreasing = TRUE, index.return = FALSE)
  cumsum(as.vector(xx.tab)) -> cs
  length(x.tab) -> lenx
  bp <- barplot(xx.tab, ylim = c(0,max(cs)),las = 2)
  lb <- seq(0,cs[lenx], l = 11)
  axis(side = 4, at = lb, labels = paste(seq(0, 100, length = 11), "%", sep = ""), las = 1, line = -1, col = "Blue", col.axis = "Red")
  for(i in 1:(lenx-1)){
    segments(bp[i], cs[i], bp[i+1], cs[i+1], col = i, lwd = 2)
  }
  title(main = mn, ...)
}


