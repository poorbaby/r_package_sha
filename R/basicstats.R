#' Basic Statistics by Group
#'
#' @param data a dataframe
#' @param x    the variable name you want to caculate on
#' @param y    the variable name group by, which shoule be a categorical variable
#' @param f    the basic r function you want to use to perform on the x variable
#'
#' @export
#'
#' @examples
#' ## Continues variable stats
#' basic_stats(sleepdata, "Weight", "DayType", max)
#' basic_stats(sleepdata, "Weight", "DayType", mean)
#' basic_stats(sleepdata, "Weight", "DayType", median)
#'
#' ## Categorical variable stats
#' basic_stats(sleepdata, "HavingDog", "DayType", table)
#'
basic_stats <- function(data, x, y, f){
  by(data[[x]], data[[y]], f)
}
