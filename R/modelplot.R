#'Simple Linear Regression Model Plot
#'
#' @param data a dataframe
#' @param x    the independent variable going on the x axis
#' @param y    the dependent variable going on the y axis
#'
#' @return a plot
#' @export
#' @import graphics
#' @importFrom stats lm coef
#' @seealso \code{\link{log_modelplot}}
#' @examples
#' modelplot(sleepduration,"Tempreture", "Heart Rate")
#'
modelplot <- function(data,x,y){
  model = lm(data[[y]]~data[[x]])
  plot(data[[x]],data[[y]],
       xlab = x,
       ylab = y,
       main = paste(y,"association with", x))
  abline(model, lty = 2, col = "red")
  eq <- paste("Predicted", y, "=", round(coef(model)[1], 4), "+ (", round(coef(model)[2], 4), ") *", x )
  mtext(eq, 3, line=-2, col = "Brown")
}

