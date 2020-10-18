#' Central Limit Theorem function
#'
#' @param n      #sample size, default
#' @param iter   #iterations, default
#' @param a
#' @param b
#'
#' @return histogram, density curve, and theoretical normal curve
#' @export
#'
#' @examples
mycltu=function(n,iter,a=0,b=10){  #default values

  ## r-random sample from the uniform
  y=runif(n*iter,a,b)

  #data placed in a matrix
  ## In matrix: iter#=columns, #n=rows
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)

  ## In vector w: apply the function mean to the columns (2) of the matrix
  w=apply(data,2,mean)

  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)

  ## Since the histogram will be a density plot we will find the max density
  ymax=max(param$density)

  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax

  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), xlab="Sample mean",
       main=paste("Histogram of sample(n) mean", "\n", "n=",n,sep=""))

  ## add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3) # add a density plot

  ## Add a theoretical normal curve
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve

  ## Add the density from which the samples were taken
  curve(dunif(x,a,b),add=TRUE,lwd=4)

}
