pizzarest = read.csv("teach/data/pizzarest.csv")
names(pizzarest)
table(pizzarest$q11)
summary(pizzarest)
head(pizzarest)

# Logistic Regression Model
fit = glm(2-q11 ~ food+atmosph+service, binomial, pizzarest)
summary(fit)
vif(fit)
# Overall Model Significance
fit$null.deviance-fit$deviance
1-pchisq(fit$null.deviance-fit$deviance, 3)

# Interpreting Parameter Estimates
summary(fit)
fit$coef
exp(fit$coef)

#Likelihood-Ratio Test for Single Parameters
drop1(fit, test="Chisq")
d2=deviance(glm(2-q11~food+service,binomial,pizzarest,subset=(!is.na(atmosph))))
d2
1-pchisq(d2-deviance(fit), 1)


