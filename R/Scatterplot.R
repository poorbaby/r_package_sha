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
#' plot_scatter(sleepdurationDay,"Weight","SleepDuration","DayType")
plot_scatter <- function(data,x,y,color){
  s <- ggplot2::ggplot(data, ggplot2::aes(
    data[[x]], data[[y]], color = data[[color]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm",se = F) +
    ggplot2::ggtitle(paste("Scatter Plot of",x,"Vs",y,"by",color)) +
    ggplot2::xlab(x) +
    ggplot2::ylab(y) +
    ggplot2::scale_colour_discrete(name  = color) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  return(s)
}






