#' calculate_PK_parameters
#'
#' 函数作用：此函数快捷计算AUC、Cmax、Cmin、CLast
#' @param df 原始数据
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
