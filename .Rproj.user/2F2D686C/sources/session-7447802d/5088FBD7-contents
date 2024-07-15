#' data manipulation
#'
#'#' 函数作用：比如将人群年龄按照是否大于等于65岁分组，此函数可以快捷实现，
#'只需要输入列名和界值
#' @param data 原始数据
#' @param columns 需要转换的列
#' @param thresholds 界值
#' @return data data.frame
#' @export
#'
#' @import data.table
#' @import ggplot2
#' @import dplyr
#' @import openxlsx
#' @import readxl
#' @import table1
#' @examples
#' df <- data.frame(
#' value1 = c(5, 12, 8, 10, 7),
#' value2 = c(18, 22, 25, 20, 30))
#' 使用group_by_threshold函数进行分组
#' df_grouped <- group_by_threshold(df, c("value1", "value2"), c(10, 20))
#' 查看结果
#' print(df_grouped)
# 定义函数
   group_by_threshold <- function(data, columns, thresholds)
  {
    library(dplyr)
    if (2>1){
      A <- .group_by_threshold(data, columns, thresholds)
    }
    return(A)
  }

.group_by_threshold <- function(data, columns, thresholds)
  {
  if (length(columns) != length(thresholds)) {
    stop("The length of columns and thresholds must be the same.")
  }

  for (i in seq_along(columns)) {
    column <- columns[i]
    threshold <- thresholds[i]
    group_column <- paste0("group_", column)
    data <- data %>%
      dplyr::mutate(!!group_column := ifelse(!!sym(column) > threshold,
                                      paste0("<", threshold),
                                      paste0("≥", threshold)))
  }

  return(data)
}
