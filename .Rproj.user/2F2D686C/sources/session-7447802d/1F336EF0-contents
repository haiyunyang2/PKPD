



#' NAC_
#'
#' 函数作用：此函数快捷计算AUC、Cmax、Cmin、CLast
#' @param data 原始数据
#' @param id_col 个体编号列
#' @param time_col  时间列
#' @param conc_col 血药浓度列
#' @param format 结果为宽型数据还是长型数据(long)，默认为宽型数据(wider)
#' @return df data.frame
#' @export
#'
#' @import data.table
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import openxlsx
#' @import readxl
#' @import table1
#' @details 维护者：杨海云
#' @examples
#' #构建示例数据
#' data<-data.frame(
#' ID = c(1,1,2,2),
#' TIME = c(1, 2, 1, 2),
#' IPRED = c(10, 15, 5, 8))
#' #示例代码
#' pk_results <- calculate_PK_parameters(data, "ID", "TIME", "IPRED")
#' print(pk_results)
# 定义函数
calculate_PK_parameters <- function(data, id_col, time_col, conc_col,format ="wider")
{
  library(dplyr)
  if (2>1){
    A <- .calculate_PK_parameters(data, id_col, time_col, conc_col,format)
  }
  return(A)
}

.calculate_PK_parameters <- function(data, id_col, time_col, conc_col,format) {
  # Ensure the data is a data.table
  setDT(data)

  # Define the helper function for deriving exposure metrics
  derive.exposure <- function(time, CP) {
    n <- length(time)
    x <- c(
      Cmax1 = max(CP),
      Clast1 = CP[n],
      Cmin1 = min(CP),
      AUC1 = sum(diff(time) * (CP[-1] + CP[-length(CP)]) / 2)
    )
    data.table(paramname=names(x), paramvalue=x)
  }

  # Apply the derive.exposure function to each ID group in the data
  results_long <- data[, derive.exposure(get(time_col), get(conc_col)), by = .(get(id_col))]

  # Convert the long format results to wide format
  results_wide <- results_long %>% pivot_wider(names_from = paramname, values_from = paramvalue)
  names(results_wide) <- c(id_col,"Cmax1","Clast1","Cmin1","AUC1")
  names(results_long) <- c(id_col,"paramname","paramvalue")
  # Return the wide format results

  if (format =="wider") {
    return(results_wide)
  } else if (format=="long") {
    return(results_long)
  } else {
    stop("Unsupported label format")
  }

}
















iv-bolus,
#'   iv-infusion or extravascular

library(dplyr)
library(ncappc)
data <- data.frame(
  ID=1,
  TIME = c(0,0.25,0.5,1,1.5,2,3,4,6,8,12,16,24),
  DOSE=100
  DV=c(0, 0.07, 0.14, 0.21, 0.24, 0.27, 0.26, 0.25, 0.22, 0.19, 0.13, 0.081, 0.033)
)
NCA <- function(data,ID,TIME,DV){
  data1 <- data %>%
    dplyr::rename(
      ID = !!sym(ID),
      TIME = !!sym(TIME),
      DV = !!sym(DV),

    )
  data <- data %>% dplyr::rename(idNmObs=!!sym(ID))
  out <- ncappc(obsFile=data1,
                onlyNCA = T,
                extrapolate = T,
                printOut = F,
                evid = FALSE,
                noPlot = T,
                idNmObs = "ID",
                timeNmObs = "TIME",
                adminType ="extravascular",
                method = "linearup-logdown",
                concNmObs ="DV")
  out <- out$ncaOutput
  out <- data.frame(out)
  return(NCA=out)
}
NAC <- NCA(data=data,TIME = "TIME",ID = "ID",DV="DV")
