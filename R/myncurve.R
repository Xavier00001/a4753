#' @title A function for making a normal curve
#'
#' @param mu   The mean
#' @param sigma   The standard deviation
#' @param a
#'
#' @return a normal curve
#' @export
#'
#' @examples
myncurve = function (mu, sigma, a){
  b = a-100
  #I'm sure there is a better way to appoint
  # b=negativeinfinity.  However, I couldn't seem to find it.
  # This seems like it would approximate decently for this lab.

  # Makes a normal curve
  curve(dnorm(x, mean=mu,sd=sigma),xlim=c(mu-3*sigma,mu+3*sigma))

  # x values corresponding to the x - cords of points on the curve
  xcurve1=seq(a, b,length=1000)

  # Y values corresponding t0 the x values
  ycurve1=dnorm(xcurve1, mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(a,xcurve1,b),c(0,ycurve1,0),col="cyan")
  area = pnorm(b,mu,sigma)-pnorm(a,mu,sigma)
  area = round(area,4)
  paste("Area= ", area, sep="")
}
