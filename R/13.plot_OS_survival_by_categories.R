
#' plot_OS_survival_by_categories
#'
#' 函数作用：此函数是可以批量绘制OS的KM曲线
#' @param data 原始数据
#' @param time_col 时间列
#' @param  event_col  删失列
#' @param categorical_vars 需要研究的暴露、协变量分组列
#' @param output_dir 保存路径
#' @param legend_titles 图例标题
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
#' # 示例使用
#' data <- data.frame(
#' time = rexp(200, rate = 0.1),
#' status = rbinom(200, 1, 0.7),
#' group = sample(c("A", "B"), 200, replace = TRUE),
#' treatment = sample(c("Control", "Treatment"), 200, replace = TRUE))
#' #示例代码
#' plot_OS_survival_by_categories(data,time_col="time",event_col="status",output_dir="",legend_titles=c("A","B"))
# 定义函数
plot_OS_survival_by_categories <- function(data, time_col, event_col, categorical_vars, output_dir, legend_titles)
{
  library(dplyr)
  if (2>1){
    A <- .plot_OS_survival_by_categories(data, time_col, event_col, categorical_vars, output_dir, legend_titles)
  }
  return(A)
}

.plot_OS_survival_by_categories <- function(data, time_col, event_col, categorical_vars, output_dir, legend_titles)
{


  for (i in seq_along(categorical_vars))
  {
    cat_var <- categorical_vars[i]

    full_model_formula <- as.formula(paste('Surv(', time_col, ',', event_col, ') ~ ',cat_var))
    fit <- surv_fit(full_model_formula, data = data)
    # 指定文件名和保存路径
    file_name <- paste(output_dir, "/", cat_var, "_survival_curve.png", sep = "")


    # 开启PNG图形设备
    P <- ggsurvplot(
      fit,
      data = data,
      pval = TRUE,
      conf.int = F,surv.median.line = "hv",risk.table = TRUE,cumcensor=T,
      xlab = "Follow up time(month)",
      ylab = "Probability of OS",
      ggtheme = theme_bw(),
      break.x.by = 4,# 设置x轴刻度间距
      break.y.by = 0.25,
      legend.title = legend_titles[i],  #自定义图例
      legend.labs = levels(as.factor(data[,cat_var])),  # 自定义图例标签
      legend.text = list(size = 12, face = "bold", color = "blue"),  # 设置图例文本的大小、字体和颜色
      # title = paste("Survival Curves for", cat_var)


    )
    splots <- list()#创建空列表
    splots[[1]] <- P  #将sexfitplot作为splots列表中的元素
    cbindplots<- arrange_ggsurvplots(splots, print = TRUE,
                                     ncol = 1, nrow = 1)  #按单行单列排列图片
    ggsave(file_name, plot=cbindplots,width =10, height =7, units="in",dpi=600)
  }
}
