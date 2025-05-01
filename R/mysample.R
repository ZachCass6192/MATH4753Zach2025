#' Mysample Function
#'
#' @param n A quantitative vector that represents
#' @param iter A quantitative vector
#' @param time A quantitative vector
#'
#' @returns A quantitative vector
#' @export
#'
#' @examples
#' mysample(n=1000, iter=30,time=1)
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){

    s=sample(1:10,n,replace=TRUE)

    sf=factor(s,levels=1:10)

    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )


    Sys.sleep(time)
  }
}
