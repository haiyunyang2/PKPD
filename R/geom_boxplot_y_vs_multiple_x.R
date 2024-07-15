
#' geom_boxplot_y_vs_multiple_x
#'
#' 函数作用：绘制多个X变量与莫个Y变量的箱型图,输出到特定路径
#' @param data 原始数据
#' @param x_vars X轴变量,为分类变量
#' @param y_var Y轴变量，为连续性变量
#' @param x_labels  定义X轴标题
#' @param y_labels  定义Y轴标题
#' @param save_path 定义图片保存路径
#' @param width1 输出图片宽度
#' @param height1 输出图片高度
#' @param show_title 是否展示图标题,默认为无图标题,若展示show_title=TRUE
#' @param sample_size 默认不展示样本量，若展示sample_size=TRUE
#' @param P 是否展示P值，若展示，填入P=TRUE
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
#' @import EnvStats
#' @import ggpubr
#' @examples
#' # 示例数据
#' data <- data.frame(
#' C_avg = rep(c("[5.2-43.1]", "[37.1-53.5]", "[53.5-97]", "[97-296]"), each = 4),
#' Response1 = rnorm(16, mean = 50, sd = 10),
#' Response1 = rnorm(16, mean = 50, sd = 10)
#' )
#' #示例代码
#' geom_boxplot_y_vs_multiple_x(data=data,
#' x_vars=c("C_avg"),
#' y_var=c("Response1")
#' y_labels="Response1",
#' x_labels=c("C_avg(μg/ml)")
#' save_path="C:/Users/haiyun.yang/Desktop/111"
#' )
# 定义函数
geom_boxplot_y_vs_multiple_x <- function(data, x_vars, y_var, y_labels,x_labels, save_path,width1=10,height1=10,sample_size,P,show_title)
  {
    library(dplyr)
    if (2>1){
      A <- .geom_boxplot_y_vs_multiple_x(data, x_vars, y_var, y_labels,x_labels, save_path,show_title,sample_size,P,show_title)
    }
    return(A)
  }

.geom_boxplot_y_vs_multiple_x <- function(data, x_vars, y_var, y_labels,x_labels, save_path,width1=10,height1=10,sample_size,P,show_title)
  {
  plots <- list()  # 初始化一个列表来存储生成的图表对象

  # 检查是否提供了自定义的X轴标题，如果没有，则使用变量名作为标题
  if (is.null(x_labels)) {
    x_labels <- x_vars
  }

  # 循环遍历每一个X变量，绘制散点图
  for (i in seq_along(x_vars)) {
    x_col <- x_vars[i]
    x_label <- x_labels[i]
    plot_title <- paste("Scatter Plot of", x_label, "vs", y_var)

    # 创建散点图
    p <- ggplot(data, aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_var))) +
      geom_boxplot()+

      geom_jitter(position = position_jitter(0.4), alpha = 0.2, size=1.5)+
      labs( x = x_label, y =y_labels) +
      theme_bw()+
      theme(legend.position = "none",
            strip.text.x = element_text(size =15),
            legend.text=element_text(size=15),
            legend.title = element_text(size=15),
            axis.title.x=element_text(size=15,vjust =-0.3),
            axis.title.y=element_text(size=15),
            axis.text  = element_text(size=15))
    if (!missing(show_title)) { p <- p + labs(title = plot_title) }
    if (!missing(sample_size)) { p <- p + stat_n_text() }
    if (!missing(P)) { p <- p+ stat_compare_means() }


    # 如果指定了保存路径，则保存图表
    if (!is.null(save_path)) {
      file_name <- paste(y_var, "vs", x_col, "scatter_plot.png", sep = "_")
      ggsave(filename = file.path(save_path, file_name), plot = p, width =width1, height = width1,dpi = 600)
    }

  }

  return(plots)
}
