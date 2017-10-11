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
#' plot_box(sleepdurationDay, x = "DayType", y= "SleepDuration")
#'
plot_box <- function(data,x,y){
  b <- ggplot2::ggplot(data, ggplot2::aes(
    data[[x]], data[[y]], color = data[[x]])) +
    ggplot2::geom_boxplot() +
    ggplot2::ggtitle(paste("Box Plot of",y,"by",x)) +
    ggplot2::xlab(x) +
    ggplot2::ylab(y) +
    ggplot2::theme_minimal() +
    ggplot2::scale_colour_discrete(name  = x)

  return(b)
}
