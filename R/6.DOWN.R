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
lower <- function(x){return(mean(x)-sd(x)*1.96)}
