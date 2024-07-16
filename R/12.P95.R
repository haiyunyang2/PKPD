#' data manipulation
#'
#' @param x character
#' @return df data.frame
#' @export
#'
#' @import data.table
#' @import ggplot2
#' @import dplyr
#' @import openxlsx
#' @import readxl
#' @import table1
#' @examples
P95 <- function(x){return(quantile(x,0.95))}
