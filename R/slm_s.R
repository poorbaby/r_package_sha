#' Fit a Simple Linear Regression Model
#'
#'
#' @description This is a function to conduct simple linear regression model on two continues variable
#' @param data a dataframe
#' @param x    the independent variable on x axis
#' @param y    the dependent variable on y axis
#'
#' @return     summary of the model
#' @export
#' @importFrom stats lm
#' @author Yunrou Gong
#' @seealso \code{\link{slm_log}}
#'
#' @examples
#' slm_s(sleepduration, "Tempreture", "SleepDuration")
slm_s <- function(data, x, y){
  model = lm(data[[y]]~data[[x]])
  return(summary(model))

}
