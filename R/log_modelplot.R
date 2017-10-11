#'Simple Linear Regression Model Plot(Log transformed)
#'
#' @param data a dataframe
#' @param x    the independent variable going on the x axis
#' @param y    the dependent variable going on the y axis
#'
#' @return a plot
#' @export
#' @import graphics
#' @importFrom stats lm
#' @seealso \code{\link{modelplot}}
#' @examples
#' modelplot(sleepduration,"Tempreture", "Heart Rate")
#'
log_modelplot <- function(data,x,y){
  model.t = lm(log(data[[y]]) ~ data[[x]])
  plot(data[[x]],data[[y]],
       xlab = x,
       ylab = y,
       main = paste(y,"association with", x))
  abline(model.t, lty = 2, col = "red")
}
