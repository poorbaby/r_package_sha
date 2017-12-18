#' Checking statistically significantly mean difference across the groups
#'
#'
#' @description To check weather there is statistically significantly difference across the groups.
#' @param data a dataframe
#' @param x    the continues variable to test weather its average is significantly difference across the groups
#' @param y    the categorical variable to group
#'
#'
#' @return a summary of anova test
#' @export
#' @importFrom stats aov
#' @examples
#' aov_t(sleepdata,"SleepDuration", "DayType")
aov_t <- function(data, x, y) {
  summary(aov(data[[x]] ~ data[[y]]))
}
