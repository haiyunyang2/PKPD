
#' plot_response_percentage
#'
#' 函数作用：以ADA阴阳性为例，此函数可以快捷统计ADA阳性在某分组中的百分比例,
#' 绘制柱状图，并输出到特定文件夹

#' @param data 原始数据
#' @param group_column X轴变量,为分类变量
#' @param response_column Y轴变量，为分类变量
#' @param target_value 定义统计Y轴变量的哪个分类
#' @param chart_title 图片标题
#' @param x_label  定义X轴标题
#' @param y_label  定义Y轴标题
#' @param file_path 定义图片保存路径
#' @param show_text 是否展示2/4(50%)的文字
#' @return
#' @export
#'
#' @import data.table
#' @import ggplot2
#' @import dplyr
#' @import openxlsx
#' @import readxl
#' @import table1
#' @import survival
#' @import survminer
#' @examples
#' # 示例数据
#' data <- data.frame(
#' C_avg = rep(c("[5.2-43.1]", "[37.1-53.5]", "[53.5-97]", "[97-296]"), each = 4),
#' Response1 = c(1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
#' Response2 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
#' )
#'  #示例代码
#'  plot_response_percentage(data, "C_avg", "Response1",
#'  target_value, "Clinical Remission per C_avg Interval",
#'  "C_avg Interval", "% Clinical Remission",
#'  "C:/Users/haiyun.yang/Desktop/111/plot.png")
# 定义函数

plot_response_percentage <- function(data, group_column,
                                     response_column,
                                     target_value,
                                     chart_title,
                                     x_label,
                                     y_label,
                                     file_path,show_text)
  {
  library(dplyr)
  if (2>1){
    A <- .plot_response_percentage(data, group_column,
                                   response_column,
                                   target_value,
                                   chart_title,
                                   x_label,
                                   y_label,
                                   file_path,show_text)
  }
  return(A)
}

.plot_response_percentage <- function(data, group_column,
                                      response_column,
                                      target_value,
                                      chart_title,
                                      x_label,
                                      y_label,
                                      file_path,
                                      show_text)
{
  # 检查列是否存在于数据中
  if (!(group_column %in% names(data)) || !(response_column %in% names(data))) {
    stop("Specified columns do not exist in the data frame.")
  }

  # 确保响应列为因子类型
  data[[response_column]] <- as.factor(data[[response_column]])

  # 检查目标值是否在响应列的级别中
  if (!target_value %in% levels(data[[response_column]])) {
    stop("Target value not found in response column levels.")
  }

  # 计算百分比
  percentage_data <- data %>%
    group_by(!!rlang::sym(group_column)) %>%
    summarise(
      Count_Target = sum(!!rlang::sym(response_column) == target_value, na.rm = TRUE),
      Total = n(),
      Percentage = Count_Target / Total
    ) %>%
    ungroup()

  # 绘制柱状图
  plot <- ggplot(percentage_data, aes(x = !!rlang::sym(group_column), y = Percentage, fill = !!rlang::sym(group_column))) +
    geom_bar(stat = "identity") +
    labs(title = chart_title, x = x_label, y = y_label) +
    theme_bw() +
    theme(legend.position = "none")+
    scale_y_continuous(labels = scales::percent,limits = c(0,1))
  if(!missing(show_text)){plot <- plot+geom_text(aes(label = paste(Count_Target, "/", Total, " (", round(Percentage* 100, 1), "%)", sep = "")),
                                    position = position_stack(vjust = 0.5),
                                    size = 3.5)}


  # 检查目标文件夹是否存在，如果不存在则创建
  if (!dir.exists(dirname(file_path))) {
    dir.create(dirname(file_path), recursive = TRUE)
  }
  QQQ <- paste0(file_path,"/",group_column," vs ",response_column,".png", sep = "")

  # 保存图表
  ggsave(QQQ, plot, width = 10, height = 6)
}
