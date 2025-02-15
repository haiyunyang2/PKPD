% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/17.run_cox_analysis.R
\name{run_cox_analysis}
\alias{run_cox_analysis}
\title{run_cox_analysis}
\usage{
run_cox_analysis(
  data,
  time_col,
  status_col,
  covariates,
  output_dir,
  xlim1,
  ticks_at1,
  multi_predictor_vars,
  width1 = 10,
  height1 = 10,
  method
)
}
\arguments{
\item{data}{data.franme}

\item{time_col}{生存时间}

\item{status_col}{事件状态}

\item{covariates}{需要分析的协变量}

\item{output_dir}{结果输出路径}

\item{xlim1}{森林图X轴刻度范围}

\item{ticks_at1}{森林图X轴刻度线}

\item{multi_predictor_vars}{填入基于逐步法筛选得到的协变量}

\item{width1}{输出的森林图图宽度}

\item{height1}{输出的森林图高度}
}
\description{
函数作用：
此函数首先进行单因素COX回归并输出单因素COX回归结果,
接下来基于逐步法自动筛选协变量,将筛选得到的多个因素进行多因素COX回归并输出结果,
此外，此函数会自动输出单因素以及多因素COX回归的森林图
}
\examples{
#构建示例数据
set.seed(123)
data <- data.frame(
time = rexp(100, rate = 0.1),  # 生存时间
status = rbinom(100, 1, 0.5),  # 事件状态
age = rnorm(100, 50, 10),      # 年龄
sex = rbinom(100, 1, 0.5),     # 性别
treatment = rbinom(100, 1, 0.6) # 治疗组别
)
#以下为示例代码
run_cox_analysis(data=data, time_col="time", status_col="status",
covariates=c("age", "sex", "treatment"),
output_dir="C:/Users/haiyun.yang/Desktop/111",xlim1=c(0,5),ticks_at1=c(0,5),width1=6,height1=6,
multi_predictor_vars=c("age", "sex", "treatment"))
}
