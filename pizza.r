pizza = read.csv("teach/304/data/pizza.csv")
names(pizza)
summary(pizza)
table(pizza$male)
table(pizza$price)

fit = glm(purchase ~ ., binomial, pizza)  # Q1
summary(fit) # Q2-Q5
names(fit)
# Q2
fit$null.deviance-fit$deviance # test statistic
1-pchisq(fit$null.deviance-fit$deviance, 2) # P-value

# Q6
eta = 1.21951448-0.25018642*8.99   # eta value
eta
1/(1+exp(-eta)) # probability estimate
predict(fit, data.frame(male=0, price=c(8.99, 11.49, 13.99)))
predict(fit, data.frame(male=0, price=c(8.99, 11.49, 13.99)), type="resp")

summary(glm(purchase~price, binomial, pizza, subset=(male==1)))  #Q7
summary(glm(purchase~price, binomial, pizza, subset=(male==0)))  #Q8
summary(glm(purchase~price*male, binomial, pizza))  #Q9


