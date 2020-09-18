#' @Title  A function for plotting a quadratic
#'
#' @param x The diameter
#'
#' @return  Height value
#' @export
#'
#' @examples
myplot=function(x){
  quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
}
