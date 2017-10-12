#' Fit a Simple Linear Regression Model (Log transformed)
#'
#' @description This is a function to conduct simple linear regression model on two continues variable,
#' Since the dependent variable may violate the normality assumption, so in this function, the dependent
#' variable is log transformed to let it follwo normal distrbution.
#'
#' @param data a dataframe
#' @param x    the independent variable
#' @param y    the dependent varible
#'
#' @return     summary of the model
#' @export
#' @importFrom stats lm
#' @author Yunrou Gong
#' @seealso \code{\link{slm_s}}
#'
#' @examples
#' slm_log(sleepduration, "Tempreture", "SleepDuration")
#'
#'
slm_log <- function(data, x, y){
  model.t = lm(log(data[[y]]) ~ data[[x]])
  return(summary(model.t))
}
