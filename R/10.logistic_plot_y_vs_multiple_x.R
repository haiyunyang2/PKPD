
#' logistic_plot_y_vs_multiple_x
#'
#' 此函数将旨在快捷进行逻辑回归拟合图，比如暴露-TEAE的图，可以批量绘制
#' @param data data.franme
#' @param x_vars 多个暴露量
#' @param y_var 结局指标
#' @param y_labels Y轴标题
#' @param x_labels 多个X轴标题
#' @param save_path 逻辑回归图保存路径
#' @param DOSE 是否将暴露箱型图分组展示，可以不填
#' @param width1 输出图片宽度
#' @param height1 输出图片高度
#' @param show_title 是否展示图标题,默认为无图标题,若展示show_title=TRUE
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
#' data <- data.frame(
#' status = rbinom(100, 1, 0.5),
#' Humidity = rnorm(100, mean = 50, sd = 10),
#' DOSE=rep(c("[5.2-43.1]", "[37.1-53.5]", "[53.5-97]", "[97-296]"), each = 25),
#' Wind_Speed = rnorm(100, mean = 15, sd = 5))
#' data$status <- as.numeric(data$status)
#' data$DOSE <- as.factor( data$DOSE)
#' #示例运行代码
#' logistic_plot_y_vs_multiple_x (data, y_var="status",
#'  x_vars=c("Humidity", "Wind_Speed"),
#'  x_labels=c("A", "Air Humidity"),
#'  y_labels="TEAE",
#'  save_path="C:/Users/haiyun.yang/Desktop/222")
# 定义函数
logistic_plot_y_vs_multiple_x <- function(data, x_vars, y_var, y_labels,x_labels = NULL, save_path = NULL,
                                          DOSE,width1=7, height1=5,show_title)
{
  library(dplyr)
  if (2>1){
    A <- .logistic_plot_y_vs_multiple_x(data, x_vars, y_var, y_labels,x_labels, save_path,
                                                    DOSE,width1, height1,show_title)
  }
  return(A)
}

.logistic_plot_y_vs_multiple_x <- function(data, x_vars, y_var, y_labels,x_labels = NULL, save_path = NULL,
                                          DOSE,width1=7, height1=5,show_title) {

  # 检查是否提供了自定义的X轴标题，如果没有，则使用变量名作为标题
  if (is.null(x_labels)) {
    x_labels <- x_vars
  }
for (i in seq_along(x_vars)) {
    x_col <- x_vars[i]
    x_label <- x_labels[i]

    # 创建散点图
    p <- ggplot(data, aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_var))) +

      geom_jitter(data = data,
                  aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_var)),
                  height = 0.05,
                  alpha = 0.5)+

    geom_smooth(data = data,
                aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_var)),
                color = "grey10",
                method = "glm",
                method.args = list(family = "binomial"))+

      labs(x = x_label, y = y_labels) +
      scale_y_continuous(breaks = seq(0,1.0,0.2),limits = c(-0.4,1.2), expand = c(0,0))+
      geom_boxplot(data = data,mapping = aes(x =!!rlang::sym(x_col),y=-0.2),width=0.2)+
      theme_bw()
    full_model_formula <- as.formula(paste0(y_var,"~",x_col))
    lgrfit1<-glm(full_model_formula, family=binomial(link = logit), data=data)
    plot_title <- paste("Scatter Plot of", x_label, "vs", y_var,"(","P=",signif(summary(lgrfit1)$coef[2,4],3),")")
    if (!missing(show_title)) { p <- p+ labs(title = plot_title) }
    # 如果指定了保存路径，则保存图表
    if (!is.null(save_path)) {
      file_name <- paste(y_var, "vs", x_col, "scatter_plot.png", sep = "_")
      ggsave(filename = file.path(save_path, file_name), plot = p, width = width1, height = height1)+theme_bw()
    }
}



  # 循环遍历每一个X变量，绘制散点图
  if(!missing(DOSE)){

  for (i in seq_along(x_vars)) {
    x_col <- x_vars[i]
    x_label <- x_labels[i]


    # 创建散点图
    p <- ggplot(data, aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_var))) +

      geom_jitter(data = data,
                  aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_var)),
                  height = 0.05,
                  alpha = 0.5)+

    geom_smooth(data = data,
                aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_var)),
                color = "grey10",
                method = "glm",
                method.args = list(family = "binomial"))+

      labs(x = x_label, y = y_labels) +
      scale_y_continuous(breaks = seq(0,1.0,0.2),limits = c(-0.4,1.2), expand = c(0,0))+
      geom_boxplot(data = data,mapping = aes(x =!!rlang::sym(x_col),y=-0.2,fill=!!rlang::sym(DOSE)),width=0.2)+
      theme_bw()
    full_model_formula <- as.formula(paste0(y_var,"~",x_col))
    lgrfit1<-glm(full_model_formula, family=binomial(link = logit), data=data)
    plot_title <- paste("Scatter Plot of", x_label, "vs", y_var,"(","P=",signif(summary(lgrfit1)$coef[2,4],3),")")
    if (!missing(show_title)) { p <- p+ labs(title = plot_title) }

    # 如果指定了保存路径，则保存图表
    if (!is.null(save_path)) {
      file_name <- paste(y_var, "vs", x_col, "scatter_plot.png", sep = "_")
      ggsave(filename = file.path(save_path, file_name), plot = p, width = width1, height = height1)+theme_bw()
    }
  }
  }
}

