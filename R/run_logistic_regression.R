#' run_logistic_regression
#'
#' @param data data.franme
#' @param outcome_var 结局指标，必须是因子
#' @param predictor_vars 需要分析的协变量
#' @param output_dir 结果输出路径
#' @param xlim1     森林图X轴刻度范围
#' @param ticks_at1 森林图X轴刻度线
#' @param multi_predictor_vars 填入基于逐步法筛选得到的协变量
#' @param width1 输出的森林图宽度
#' @param height1 输出的森林图高度
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
#' @details 版权作者:杨海云
#' 未禁许可，禁止外传
#' @examples
#' #构建示例数据
#' set.seed(123)
#' data <- data.frame(
#' status = rbinom(100, 1, 0.5),  # 结局事件
#' age = rnorm(100, 50, 10),      # 年龄
#' sex = rbinom(100, 1, 0.5),     # 性别
#' treatment = rbinom(100, 1, 0.6) # 治疗组别
#' )
#' #以下为示例代码
#'  run_logistic_regression(data=data, outcome_var="status",
#'  predictor_vars=c("age", "sex", "treatment"),
#'  output_dir="C:/Users/haiyun.yang/Desktop/222",xlim1=c(0,5),ticks_at1=c(0,5)
#'  ,multi_predictor_vars=c("age"))
# 定义函数
run_logistic_regression <- function(data, outcome_var, predictor_vars, output_dir,xlim1=c(0,5),ticks_at1=c(0,5),
                                    multi_predictor_vars,width1=10,height1=10)
{
  library(dplyr)
  if (2>1){
    A <- .run_logistic_regression(data, outcome_var, predictor_vars, output_dir,xlim1,ticks_at1,multi_predictor_vars,
                                  width1,height1)
  }
  return(A)
}

.run_logistic_regression <- function(data, outcome_var, predictor_vars, output_dir,xlim1,ticks_at1,multi_predictor_vars,
                                     width1,height1) {
  # 确保输出目录存在
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  explanatory = predictor_vars
  explanatory_multi = predictor_vars
  dependent = outcome_var
  data%>%
    finalfit(dependent, explanatory, explanatory_multi) -> t4
  library(stringr)
  t4 <- t4[,c(1:5)]

  t4$OR <- sub("\\s*([0-9.]+).*", "\\1", t4$`OR (univariable)`)
  t4$OR_CI <- sub(".*?\\(([^-]+)-.*", "\\1", t4$`OR (univariable)`)
  t4$OR_UI <- str_extract(t4$`OR (univariable)`,"(?<=-)[0-9.]+(?=,)")
  t4$OR <- gsub("-",NA,t4$OR)
  t4$OR_CI <- gsub("-",NA,t4$OR_CI)
  t4$OR_UI <- gsub("-",NA,t4$OR_UI)
  write.csv(t4, file.path(output_dir, "Univariate_logistic_Results_Summary.csv"),
            row.names = FALSE,na="")
  t4$group <- t4$` `
  t4$` ` <- paste(rep(" ", nrow(t4)), collapse = " ")
  t4$character <- t4$`Dependent: status`
  colnames(t4)
  t4$OR <- as.numeric(t4$OR)
  t4$OR_CI <- as.numeric(t4$OR_CI)
  t4$OR_UI <- as.numeric(t4$OR_UI)
  p <- forest(t4[,c(10,9,3,4,2,5)],
             est = t4$OR,       #效应值
              lower = t4$OR_CI,     #可信区间下限
              upper = t4$OR_UI,      #可信区间上限
              # sizes = dt$se,     #黑框的大小
             ci_column =5,   #在那一列画森林图，要选空的那一列
              ref_line = 1,
              # arrow_lab = c(" ", ""),
              xlim = xlim1,
            ticks_at = ticks_at1,
             footnote = " ")
QQQQ=paste(output_dir, "/单因逻辑回归森林图.png", sep="")
 ggsave(QQQQ,p,width =width1,height =height1,dpi = 600)

  # 使用所有预测变量创建一个全模型
  full_formula <- as.formula(paste(outcome_var, "~", paste(predictor_vars, collapse = " + ")))
  full_model <- glm(full_formula, data = data, family = binomial())

  # 使用逐步法进行变量选择
  step_model <- stepAIC(full_model, selection="bidirection",select = c("AIC"),method="efron",sle=0.05,sls=0.05,trace = FALSE)

  # 基于筛选后的模型进行多因素逻辑回归
  final_model <- glm(formula(step_model), data = data, family = binomial())
  final_model_summary <- summary(final_model)
  if(!missing(multi_predictor_vars)){
    data%>%
      finalfit(dependent, multi_predictor_vars, multi_predictor_vars) -> t4
    library(stringr)
    t4 <- t4[,c(1:4,6)]

    t4$OR <- sub("\\s*([0-9.]+).*", "\\1", t4$`OR (multivariable)`)
    t4$OR_CI <- sub(".*?\\(([^-]+)-.*", "\\1", t4$`OR (multivariable)`)
    t4$OR_UI <- str_extract(t4$`OR (multivariable)`,"(?<=-)[0-9.]+(?=,)")
    t4$OR <- gsub("-",NA,t4$OR)
    t4$OR_CI <- gsub("-",NA,t4$OR_CI)
    t4$OR_UI <- gsub("-",NA,t4$OR_UI)
    write.csv(t4, file.path(output_dir, "Final_logistic_Multivariate_Model.csv.csv"),
              row.names = FALSE,na="")
    t4$group <- t4$` `
    t4$` ` <- paste(rep(" ", nrow(t4)), collapse = " ")
    t4$character <- t4$`Dependent: status`
    colnames(t4)
    t4$OR <- as.numeric(t4$OR)
    t4$OR_CI <- as.numeric(t4$OR_CI)
    t4$OR_UI <- as.numeric(t4$OR_UI)
    p <- forest(t4[,c(10,9,3,4,2,5)],
                est = t4$OR,       #效应值
                lower = t4$OR_CI,     #可信区间下限
                upper = t4$OR_UI,      #可信区间上限
                # sizes = dt$se,     #黑框的大小
                ci_column =5,   #在那一列画森林图，要选空的那一列
                ref_line = 1,
                # arrow_lab = c(" ", ""),
                xlim = xlim1,
                ticks_at = ticks_at1,
                footnote = " ")
    QQQQ=paste(output_dir, "/多因逻辑回归森林图.png", sep="")
    ggsave(QQQQ,p,width=width1,height=height1,dpi = 600)

  }
  # 将多因素逻辑回归结果输出到CSV文件
  final_output_path <- file.path(output_dir, "基于逐步法筛选得到的协变量.csv")
  write.csv(coef(final_model_summary), final_output_path)
}
