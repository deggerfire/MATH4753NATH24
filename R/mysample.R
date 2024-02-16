#' My Sample
#'
#' @param n Number of pulls per set
#' @param iter Number of time to run the experiment
#' @param time does nothing
#'
#' @export
#'
#' @examples
#' mysample(5)
mysample <- function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
    main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
    ylim=c(0,0.2))
  }
}
