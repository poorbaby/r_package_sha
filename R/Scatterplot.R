#' Scatter Plot by Group
#'
#' @param data  a dataframe
#' @param x     The variable is going on the x axis
#' @param y     The variable is going on the y axis
#' @param color color by this variable
#' @return a plot
#' @export
#' @import ggplot2
#' @examples
#' plot_scatter(sleepdata,"Weight","SleepDuration","DayType")
plot_scatter <- function(data,x,y,color){
  sch <- c("Date", "Weight(kg)", "Sleep Duration(hr)", "Tempreture(C)", "Activity(steps)",
           "Heart Rate(bpm)","Having Dog", "Day Type")
  lookup <- cbind.data.frame(name_unit=sch, origin_name=colnames(data))
  s <- ggplot2::ggplot(data, ggplot2::aes(
    data[[x]], data[[y]], color = data[[color]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm",se = F) +
    ggplot2::ggtitle(paste("Scatter Plot of",x,"Vs",y,"by",color)) +
    ggplot2::xlab(lookup$name_unit[lookup$origin_name==x]) +
    ggplot2::ylab(lookup$name_unit[lookup$origin_name==y]) +
    ggplot2::scale_colour_discrete(name  = lookup$name_unit[lookup$origin_name==color]) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  return(s)
}






