#' MoneyBallDataPrep is a function that Preps the data for model verification
#'
#' @param Path Strig with Path to file
#'
#' @return Data frame 
#' @export
#'
#' @examples MoneyBallDataPrep('MoneyBall_Test.csv')
#' @author Sri Seshadri MSPA
MoneyBallDataPrep <- function(Path) {
  # Library calls
  library(dplyr)
  library(magrittr)
  library(VIM)
  
  # read data in
  MB2 <- read.csv(file = Path , header = T)
  
  # Identity columns positions of those that are not required.
  NotRequired <- c("INDEX", "TEAM_BATTING_HBP")
  NotRequiredPos <- which(colnames(MB2) %in% NotRequired)
  
  # Impute Data

  MB2 <- kNN(data = MB2,variable = colnames(MB2)[NotRequiredPos*-1], k = 5)
  
  # Attrubute and indicator variables
  
  MB2 <- MB2  %>% 
    mutate(BatHR_Filter = as.logical(ifelse(TEAM_BATTING_HR <= 59, 1,0))) %>% 
    mutate(BatSO_Filter = as.logical(ifelse(TEAM_BATTING_SO <= 250, 1,0))) %>% 
    mutate(PitSO_Filter = as.logical(ifelse(TEAM_PITCHING_SO > 2000, 1, 0))) %>% 
    mutate(Pitch_H_Outlier = as.logical(ifelse(TEAM_PITCHING_H >= 1890, 1,0)))
  
  Pitch_H_outlierT <- MB2$INDEX[MB2$Pitch_H_Outlier]
  BatSO_Filter_T <- MB2$INDEX[MB2$BatSO_Filter]
  PitSOFilter_T <- MB2$INDEX[MB2$PitSO_Filter]
  
  MB2 <- MB2 %>% 
    mutate (AltSlope = as.logical(ifelse(INDEX %in% setdiff(setdiff(Pitch_H_outlierT,BatSO_Filter_T), PitSOFilter_T), 1,0)))
  
 # Get required columns
  reqcols <- colnames(MB2)[c(grep("_imp",colnames(MB2))*-1,NotRequiredPos*-1)]
  
 MB2 %>% dplyr::select(reqcols)
}