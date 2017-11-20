#' Retrieve equation as a text for reporting and model exporting
#'
#' @param target as String indicating target variable 
#' @param fit lm,glm model object
#' @param rounding numeric, round coefficients to as many digits
#'
#' @return formula as text
#' @export
#'
#' @examples
#' test <- lm(Sepal.Width ~ Sepal.Length, data = iris)
#' getequation("Sepal.Width",test,2)
#' "Sepal.Width = 3.42 - 0.06 * Sepal.Length"
#' 
#' @author Sri Seshadri srivathsanseshadri2019.u.northwestern.edu
#' 
getequation <- function(target,fit, rounding = 2){
  if(!class(fit) == "zeroinfl") {
    formulae <- as.character(formula(fit))[3]
  predictors <- unlist(strsplit(formulae,split = "+", fixed = T))
  coef <- round(coefficients(fit),rounding)
  signs <- ifelse(sign(coef)==1,"+", "-")
  Betas <- paste(abs(coef[2:length(coef)]),"*",predictors)
  equation <- paste(target, "=", paste(coef[1],paste(paste(signs[2:length(signs)],Betas),collapse = " ")))
  return(equation)
  }
  else{
    formulae <- as.character(formula(fit))[3]
    predictors <- unlist(strsplit(formulae,split = "|", fixed = T))
    countcomp <- predictors[1]
    logistcomp <- predictors[2]
    predictors.comp <- unlist(strsplit(countcomp,split = "+", fixed = T))
    predictors.logis <- unlist(strsplit(logistcomp,split = "+", fixed = T))
    coef <- round(coefficients(fit),rounding)
    coef.count <- coef[0:length(predictors.comp)+1]
    coef.logis <- rev(coef[length(predictors.comp)+2:length(predictors.logis)+1])
    signs.count <- ifelse(sign(coef.count)==1,"+", "-")
    signs.logis <- ifelse(sign(coef.logis)==1,"+", "-")
    Betas.count <- paste(abs(coef.count[2:length(coef.count)]),"*",predictors.comp)
    Betas.logis <- paste(abs(coef.logis[2:length(coef.logis)]),"*",predictors.logis)
    equation.count <- paste(target, "=", paste(coef.count[1],paste(paste(signs.count[2:length(signs.count)],Betas.count),collapse = " ")))
    equation.logis <- paste(target, "=", paste(coef.logis[1],paste(paste(signs.logis[2:length(signs.logis)],Betas.logis),collapse = " ")))
    equation <- paste(equation.count, "|", equation.logis)
  }
  
}