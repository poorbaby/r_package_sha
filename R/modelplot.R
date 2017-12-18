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
#' modelplot(sleepduration,"Tempreture", "SleepDuration")
#'
modelplot <- function(data,x,y){
  model = lm(data[[y]]~data[[x]])
  sch <- c("Date", "Weight(kg)", "Sleep Duration(hr)", "Tempreture(°C)", "Activity(steps)",
           "Heart Rate(bpm)","Having Dog", "Day Type")
  lookup <- cbind.data.frame(name_unit=sch, origin_name=colnames(data))
  plot(data[[x]],data[[y]],
       xlab = lookup$name_unit[lookup$origin_name==x],
       ylab = lookup$name_unit[lookup$origin_name==y],
       main = paste(y,"association with", x))
  abline(model, lty = 2, col = "red")
  slope <- paste("Slope = ", round(coef(model)[2], 2))
  mtext(slope, 3, line=-2, col = "Blue")
}

