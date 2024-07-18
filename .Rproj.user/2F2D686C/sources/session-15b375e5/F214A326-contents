
#' 函数作用：实现NCA分析
#' @param data 原始数据
#' @param ID 个体编号列
#' @param TIME  时间列
#' @param DV 血药浓度列
#' @param adminType 给药方式,可以从"extravascular","iv-infusion","iv-bolus"选择一种，若为静脉输注需要填写输注时间::Infusion_duration=XX
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
#' @importncappc
#' @examples
#' #构建示例数据
#' data <- data.frame(
#' ID=1,
#' TIME = c(0,0.25,0.5,1,1.5,2,3,4,6,8,12,16,24),
#' DOSE=100,
#' DV=c(0, 0.07, 0.14, 0.21, 0.24, 0.27, 0.26, 0.25, 0.22, 0.19, 0.13, 0.081, 0.033)
#' )
#' #示例代码
#' NAC <- NCA(data=data,TIME = "TIME",ID = "ID",DV="DV",adminType="extravascular")
# 定义函数
NCA <- function(data,ID,TIME,DV,DOSE,adminType="extravascular",Infusion_duration){
  data1 <- data %>%
    dplyr::rename(
      ID = !!sym(ID),
      TIME = !!sym(TIME),
      DV = !!sym(DV),
    )
  if(adminType=="extravascular"){
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
                  concNmObs ="DV",
                  doseAmtNm = "DOSE")}
  if(adminType=="iv-infusion"){
    out <- ncappc(obsFile=data1,
                  onlyNCA = T,
                  extrapolate = T,
                  printOut = F,
                  evid = FALSE,
                  noPlot = T,
                  idNmObs = "ID",
                  timeNmObs = "TIME",
                  adminType ="iv-infusion",
                  method = "linearup-logdown",
                  concNmObs ="DV",
                  doseAmtNm = "DOSE",
                  TI=Infusion_duration)}
  if(adminType=="iv-bolus"){
    out <- ncappc(obsFile=data1,
                  onlyNCA = T,
                  extrapolate = T,
                  printOut = F,
                  evid = FALSE,
                  noPlot = T,
                  idNmObs = "ID",
                  timeNmObs = "TIME",
                  adminType ="iv-bolus",
                  method = "linearup-logdown",
                  concNmObs ="DV",
                  doseAmtNm = "DOSE")}

  out <- data.frame(out$ncaOutput)
  return(NCA=out)
}
