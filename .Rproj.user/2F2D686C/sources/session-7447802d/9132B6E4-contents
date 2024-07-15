
#'convert_three_Quartiles_value
#'
#' 函数作用：此函数是将数据框中的多列批量转化为三分位数区间，分组列按照数值区间表示，例如将Cmax按照三分卫数区间分组
#' @param df 需要转换的数据框
#' @param column_names 需要转换的列,可以为多列
#' @param suffix  新形成分组列标签后缀
#' @param label_format 区间展示形式，可以从Q、Q_numeric、numeric中选择，默认为Q_numeric
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
#' #构建示例数据
#' df <- data.frame(
#' value1 = c(5, 12, 8, 15, 7),
#' value2 = c(18, 22, 25, 10, 30)
#' )
#' #对指定列进行四分位数转换
#' df <- convert_three_Quartiles_value(df, column_names=c("value1", "value2"))
# 定义函数
convert_three_Quartiles_value <- function(df, column_names, suffix = "_quartiles",label_format ="Q_numeric")
{
  library(dplyr)
  if (2>1){
    A <- .convert_three_Quartiles_value(df, column_names, suffix = "_quartiles",label_format)
  }
  return(A)
}

.convert_three_Quartiles_value<- function(df, column_names, suffix = "_quartiles", label_format) {
  # 检查所有列名是否在数据框中
  if (!all(column_names %in% names(df))) {
    stop("One or more column names do not exist in the data frame")
  }

  # 对每一个指定的列进行四分位数转换
  for (column_name in column_names) {
    result_column_name <- paste(column_name, suffix, sep = "")
    breaks <- quantile(df[[column_name]], probs = c(0, 0.33, 0.66, 1), na.rm = TRUE, include.lowest = TRUE)

    if (label_format == "Q_numeric") {
      labels <- paste0("Q", 1:3, "[", head(breaks, -1), ", ", tail(breaks, -1), "]")
      labels <- gsub("\\]", "]", labels)  # 最后一个区间闭合
    } else if (label_format == "numeric") {
      labels <- paste0("[", head(breaks, -1), ", ", tail(breaks, -1), "]")
      labels <- gsub("\\]", "]", labels)  # 最后一个区间闭合
    } else if (label_format == "Q") {
      labels = c("Q1(0-33%)", "Q2(34-66%)", "Q3(67-100%)")
    }

    else {
      stop("Unsupported label format")
    }

    df[[result_column_name]] <- cut(df[[column_name]],
                                    breaks = breaks,
                                    labels = labels,
                                    include.lowest = TRUE,
                                    right = FALSE)
  }
  return(df)
}
