#' Box Plot by Group
#'
#' @param data a dataframe
#' @param x    The vairbale on the x axis and the box plot group by this variable
#' @param y    The variale on the y axis
#'
#' @return     a plot
#' @export
#' @import ggplot2
#' @examples
#' plot_box(sleepdata, x = "DayType", y= "SleepDuration")
#'
plot_box <- function(data,x,y){
  sch <- c("Date", "Weight(kg)", "Sleep Duration(hr)", "Tempreture(C)", "Activity(steps)",
           "Heart Rate(bpm)","Having Dog", "Day Type")
  lookup <- cbind.data.frame(name_unit=sch, origin_name=colnames(data))
  b <- ggplot2::ggplot(data, ggplot2::aes(
    data[[x]], data[[y]], color = data[[x]])) +
    ggplot2::geom_boxplot() +
    ggplot2::ggtitle(paste("Box Plot of",y,"by",x)) +
    ggplot2::xlab(lookup$name_unit[lookup$origin_name==x]) +
    ggplot2::ylab(lookup$name_unit[lookup$origin_name==y]) +
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_discrete(name = lookup$name_unit[lookup$origin_name==x])

  return(b)
}
