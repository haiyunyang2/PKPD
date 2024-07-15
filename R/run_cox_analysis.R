#' run_cox_analysis
#'
#' 函数作用：
#' 此函数首先进行单因素COX回归并输出单因素COX回归结果,
#' 接下来基于逐步法自动筛选协变量,将筛选得到的多个因素进行多因素COX回归并输出结果,
#' 此外，此函数会自动输出单因素以及多因素COX回归的森林图
#' @param data data.franme
#' @param time_col 生存时间
#' @param status_col  事件状态
#' @param covariates 需要分析的协变量
#' @param output_dir 结果输出路径
#' @param xlim1     森林图X轴刻度范围
#' @param ticks_at1 森林图X轴刻度线
#' @param multi_predictor_vars 填入基于逐步法筛选得到的协变量
#' @param width1 输出的森林图图宽度
#' @param height1 输出的森林图高度
#' @details 版权作者:杨海云
#' 未禁许可，禁止外传
#' @export
#'
#' @import data.table
#' @import ggplot2
#' @import dplyr
#' @import openxlsx
#' @import readxl
#' @import forestploter
#' @import table1
#' @import survival
#' @import MASS
#' @import finalfit
#' @import survminer
#' @examples
#' #构建示例数据
#' set.seed(123)
#' data <- data.frame(
#' time = rexp(100, rate = 0.1),  # 生存时间
#' status = rbinom(100, 1, 0.5),  # 事件状态
#' age = rnorm(100, 50, 10),      # 年龄
#' sex = rbinom(100, 1, 0.5),     # 性别
#' treatment = rbinom(100, 1, 0.6) # 治疗组别
#' )
#' #以下为示例代码
#' run_cox_analysis(data=data, time_col="time", status_col="status",
#' covariates=c("age", "sex", "treatment"),
#' output_dir="C:/Users/haiyun.yang/Desktop/111",xlim1=c(0,5),ticks_at1=c(0,5),width1=6,height1=6,
#' multi_predictor_vars=c("age", "sex", "treatment"))
# 定义函数
  run_cox_analysis <- function(data, time_col, status_col, covariates, output_dir, xlim1=c(0,5),ticks_at1=c(0,5),
                               multi_predictor_vars,
                               width1=10,height1=10)
  {
    library(dplyr)
    if (2>1){
      A <- .run_cox_analysis(data, time_col, status_col, covariates, output_dir,xlim1,ticks_at1,
                             multi_predictor_vars,width1,height1)
    }
    return(A)
  }

.run_cox_analysis <- function(data, time_col, status_col, covariates, output_dir,xlim1,ticks_at1,
                              multi_predictor_vars,width1=10,height1=10)
  {
  explanatory = covariates
  dependent = paste('Surv(', time_col, ',', status_col,")")
  data %>%
    finalfit(dependent, explanatory) -> t4

  t4 <- t4[,c(1:5)]

  t4$HR <- sub("\\s*([0-9.]+).*", "\\1", t4$`HR (univariable)`)
  t4$HR_CI <- sub(".*?\\(([^-]+)-.*", "\\1", t4$`HR (univariable)`)
  t4$HR_UI <- str_extract(t4$`HR (univariable)`,"(?<=-)[0-9.]+(?=,)")
  t4$HR <- gsub("-",NA,t4$HR)
  t4$HR_CI <- gsub("-",NA,t4$HR_CI)
  t4$HR_UI <- gsub("-",NA,t4$HR_UI)
  write.csv(t4, file.path(output_dir, "COX_univariate_results.csv"),
            row.names = FALSE,na="")
  t4$group <- t4$` `
  t4$` ` <- paste(rep(" ", nrow(t4)), collapse = " ")
  t4$character <- t4$`Dependent: Surv( time , status )`
  colnames(t4)
  t4$HR <- as.numeric(t4$HR)
  t4$HR_CI <- as.numeric(t4$HR_CI)
  t4$HR_UI <- as.numeric(t4$HR_UI)
  p <- forest(t4[,c(10,9,3,2,4)],
              est = t4$HR,       #效应值
              lower = t4$HR_CI,     #可信区间下限
              upper = t4$HR_UI,      #可信区间上限
              # sizes = dt$se,     #黑框的大小
              ci_column =4,   #在那一列画森林图，要选空的那一列
              ref_line = 1,
              # arrow_lab = c(" ", ""),
              xlim = xlim1,
              ticks_at = ticks_at1,
              footnote = " ")
  QQQQ=paste(output_dir, "/单因素COX回归森林图.png", sep="")
  ggsave(QQQQ,p,width = width1,height = height1,dpi = 600)
  # 多因素Cox回归，使用逐步法选择变量
  full_model_formula <- as.formula(paste('Surv(', time_col, ',', status_col, ') ~ ', paste(covariates, collapse = "+")))
  full_model <- coxph(full_model_formula, data = data)
  stepwise_model <- stepAIC(full_model, selection="bidirection",select = c("AIC"),method="efron",sle=0.05,sls=0.05, trace = T)
  final_model_summary <- summary(stepwise_model)
  coef <- final_model_summary$coefficients
  # 整理单因素结果

  # 保存多因素模型结果到CSV
  multivariate_results <- as.data.frame(final_model_summary$coefficients)

  write.csv(multivariate_results, file=paste(output_dir, "/基于逐步法筛选得到的协变量.csv", sep=""), row.names=TRUE)

  # 打印信息确认文件保存
  print(paste("Univariate results saved to", output_dir, "/COX_univariate_results.csv"))
  print(paste("Multivariate results saved to", output_dir, "/C0X_multivariate_results.csv"))
  if(!missing(multi_predictor_vars )){
    explanatory = multi_predictor_vars
    dependent = paste('Surv(', time_col, ',', status_col,")")
    data %>%
      finalfit(dependent, explanatory) -> t4

    t4 <- t4[,c(1:5)]

    t4$HR <- sub("\\s*([0-9.]+).*", "\\1", t4$`HR (multivariable)`)
    t4$HR_CI <- sub(".*?\\(([^-]+)-.*", "\\1", t4$`HR (multivariable)`)
    t4$HR_UI <- str_extract(t4$`HR (multivariable)`,"(?<=-)[0-9.]+(?=,)")
    t4$HR <- gsub("-",NA,t4$HR)
    t4$HR_CI <- gsub("-",NA,t4$HR_CI)
    t4$HR_UI <- gsub("-",NA,t4$HR_UI)
    write.csv(t4, file.path(output_dir, "Final_COX_multivariate_results.csv"),
              row.names = FALSE,na="")
    t4$group <- t4$` `
    t4$` ` <- paste(rep(" ", nrow(t4)), collapse = " ")
    t4$character <- t4$`Dependent: Surv( time , status )`
    colnames(t4)
    t4$HR <- as.numeric(t4$HR)
    t4$HR_CI <- as.numeric(t4$HR_CI)
    t4$HR_UI <- as.numeric(t4$HR_UI)
    p <- forest(t4[,c(10,9,3,2,5)],
                est = t4$HR,       #效应值
                lower = t4$HR_CI,     #可信区间下限
                upper = t4$HR_UI,      #可信区间上限
                # sizes = dt$se,     #黑框的大小
                ci_column =4,   #在那一列画森林图，要选空的那一列
                ref_line = 1,
                # arrow_lab = c(" ", ""),
                xlim = xlim1,
                ticks_at = ticks_at1,
                footnote = " ")
    QQQQ=paste(output_dir, "/多因素COX回归森林图.png", sep="")
    ggsave(QQQQ,p,width = width1,height = height1,dpi = 600)
















  }




}



