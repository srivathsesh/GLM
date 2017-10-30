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
  formulae <- as.character(formula(fit))[3]
  predictors <- unlist(strsplit(formulae,split = "+", fixed = T))
  coef <- round(coefficients(fit),rounding)
  signs <- ifelse(sign(coef)==1,"+", "-")
  Betas <- paste(abs(coef[2:length(coef)]),"*",predictors)
  equation <- paste(target, "=", paste(coef[1],paste(paste(signs[2:length(signs)],Betas),collapse = " ")))
  return(equation)
}