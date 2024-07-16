
#' plot_stacked_categories
#'
#' 此函数将旨在绘制堆积柱状图
#' @param data data.franme
#' @param x_vars 分组变量
#' @param y_var 结局指标
#' @param y_labels Y轴标题
#' @param x_labels X轴标题
#' @param save_path 逻辑回归图保存路径
#' @param width1 输出图片宽度
#' @param height1 输出图片高度
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
#' #构建示例数据
#' data<- data.frame(
#' Group = rep(c("A", "B", "C"), each = 10),
#' Group1 = rep(c("A", "B", "C"), each = 10),
#' Category = sample(c("Pass", "Fail"), 30, replace = TRUE, prob = c(0.7, 0.3))
#' )
#' #示例代码
#' plot_stacked_categories(data, x_vars=c"Group",
#' y_var="Category", y_labels="zzz",x_labels=c"BBB",
#' save_path="C:/Users/haiyun.yang/Desktop/222",
#' width1=8, height1=5)
# 定义函数
plot_stacked_categories <- function(data, x_vars, y_var, y_labels,x_labels = NULL, save_path = NULL,
                                        width1=7, height1=5)
{
  library(dplyr)
  if (2>1){
    A <- .plot_stacked_categories(data, x_vars, y_var, y_labels,x_labels, save_path,
                                        width1, height1)
  }
  return(A)
}

.plot_stacked_categories <- function(data, x_vars, y_var, y_labels,x_labels = NULL, save_path = NULL,
                                           width1=10, height1=6) {

  # 检查是否提供了自定义的X轴标题，如果没有，则使用变量名作为标题
  for (i in seq_along(x_vars))
    {
    x_col <- x_vars[i]
    x_label <- x_labels[i]
    plot_title <- paste("Scatter Plot of", x_label, "vs", y_var)


    data <- data %>%
      count(!!sym(y_var), !!sym(x_col)) %>%
      group_by(!!sym(x_col)) %>%
      mutate(Percentage = n / sum(n) * 100)

    # 创建散点图
    p <- ggplot(data = data,aes(x=!!rlang::sym(x_col),y=Percentage,fill=!!rlang::sym(y_var)))+
      geom_bar(stat = "identity",
               position = "stack")+
      labs(title = plot_title, x = group_column, y = "Percentage (%)") +theme_bw()# 使用温和的颜色填充
    # 如果指定了保存路径，则保存图表
    if (!is.null(save_path)) {
      file_name <- paste(y_var, "vs", x_col, "scatter_plot.png", sep = "_")
      ggsave(filename = file.path(save_path, file_name), plot = p, width = width1, height = height1,dpi = 600)
                             }
  }

}


