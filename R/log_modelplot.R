#'Simple Linear Regression Model Plot(Log transformed)
#'
#' @param data a dataframe
#' @param x    the independent variable going on the x axis
#' @param y    the dependent variable going on the y axis
#'
#' @return a plot
#' @export
#' @import graphics
#' @importFrom stats lm coef
#' @seealso \code{\link{modelplot}}
#' @examples
#' log_modelplot(sleepduration,"Tempreture", "SleepDuration")
#'
log_modelplot <- function(data,x,y){
  model.t = lm(log(data[[y]]) ~ data[[x]])
  plot(data[[x]], log(data[[y]]),
       xlab = x,
       ylab = paste("log", y),
       main = paste(y,"association with", x))
  abline(model.t, lty = 2, col = "blue")
  eq <- paste("Predicted log", y, "=", round(coef(model.t)[1], 4), "+ (", round(coef(model.t)[2], 6), ") *", x )
  mtext(eq, 3, line=-2, col = "Brown")
}
