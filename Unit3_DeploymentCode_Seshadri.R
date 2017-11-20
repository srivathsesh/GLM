# This is a model deployment code for wine test
# read in libraries
library(dplyr)
library(magrittr)
library(VIM)
# read in data
winedata.test <- read.csv("wine_test.csv")

# impute data
winedata.test.clean <- winedata.test[,-2]
winedata.test.clean <- VIM::kNN(winedata.test.clean)

# predict TARGET from model object - for verification purpose
TARGET <- round(predict(poisson.zif.mdl2, newdata = winedata.test.clean, type = "response"),0)
hist(TARGET)

# model deployment from logistic model to predict zero inflation
eta <- 22.15 - 23.7 * winedata.test.clean$STARS
prob.no.sale <- (1 + exp(-1*eta))^-1
prob.sale <- 1 - prob.no.sale
plot(prob.sale)

# predict counts
predictedcounts <- round(exp(1.33 + 0.25* winedata.test.clean$LabelAppeal) *  prob.sale,0)
hist(predictedcounts)

# writing predicted counts to file
winedata.test$TARGET <- predictedcounts
write.csv(file = "wine_test_SESHADRI.csv",winedata.test[,c("INDEX","TARGET")])
