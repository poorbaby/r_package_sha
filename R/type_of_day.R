#' Classify the Day Type
#'
#' @param df a dataframe
#' @param x  The variable want to classify
#' @param v1 The tag name for weekday
#' @param v2 The tag name for weekend
#' @description  Classfify the variable by type "weekday" and  "weekend"
#' @return a vector
#' @export
#' @examples
#' classifyday(sleepduration,"DayofWeek")
#'
classifyday <- function(df,x,v1= "weekday",v2="weekend") {
  tem<- ifelse( df[[x]] %in% c( "Mon","Tue", "Wed", "Thu","Fri"), v1, v2)
  return(tem)
}





