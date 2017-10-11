#' Pair Correlation Calculation
#' @description To calculate all the correlation between two continues variable in a given dataset.
#'
#' @param data a dataframe
#' @param digits the correlation value round to the given digits, by default the digits equal to 2
#' @return a matrix
#' @export
#' @importFrom stats cor
#' @examples
#' paircorr(sleepduration)
paircorr <- function(data, digits = 2) {
  con_var <- character()
  for (i in colnames(data)) {
    if(class(data[[i]]) %in% c("numeric","integer")) {
      con_var <- c(con_var,i)
    }
  }
  r <- cor(data[,con_var])
  return(round(r, digits = digits))
}
