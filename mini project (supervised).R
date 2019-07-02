table <- read.csv("USAedu.csv", header = T, sep = ",")
names(table)
head(table)
attach(table)
library (ISLR)
library (xtable)
library(leaps)


#1 Multi regression
summary(table$gdp)
pairs(table)
pairs(~gdp + bachelor + rate123 + west + north+ mid , table)
par(mfrow=c(1,3))
boxplot(gdp,main = "GDP per Capita by States")
boxplot(bachelor,main = "College Degree Attainment Rate")
boxplot(rate123,main = "College Numbers per Million Population")
train <- sample(1:nrow(table), nrow(table) / 2)
table.train <- table[train, -20]
table.test <- table[-train, -20]
regfit.full<-
        regsubsets(gdp~bachelor+rate123+west+north+mid+bachelor*west+bachelor*mid+bachelor*north,
                   data=table.train ,nvmax =8)
reg.summary<-summary(regfit.full)
reg.summary$adjr2
reg.summary$bic
reg.summary$cp
reg.summary$rss

coef(regfit.full,2)
par(mfrow =c(2,2))
plot(summary(regfit.full)$rss, typ='l',
     xlab='number of variable',
     ylab = expression(paste('RSS')),
     main = 'Best subset selection',)
plot(summary(regfit.full)$adjr2, typ='l',
     xlab='number of variable',
     ylab = expression(paste('Adjusted', R^2)),
     main = 'Best subset selection',)
plot(summary(regfit.full)$bic, typ='l',
     xlab='number of variable',
     ylab = expression(paste('BIC')),
     main = 'Best subset selection',)
plot(summary(regfit.full)$cp, typ='l',
     xlab='number of variable',
     ylab = expression(paste('CP')),
     main = 'Best subset selection',)
#2# LASSO
lm.fit <-lm(gdp~bachelor+rate123,data=table.train)
summary(lm.fit)
lm.fit1<-
        lm(gdp~bachelor+rate123+west+north+mid+bachelor*west+bachelor*mid+bachelor*north,data=table.train)
summary(lm.fit1)

yhat <- predict(lm.fit, newdata = table.test)
mean((yhat - table.test$gdp)^2)
library (glmnet)
train.mat <- model.matrix(gdp~ bachelor+rate123+west+north+mid+bachelor*west+bachelor*mid+bachelor*north,data=table.train )
test.mat <- model.matrix(gdp~ bachelor+rate123+west+north+mid+bachelor*west+bachelor*mid+bachelor*north,data=table.test )
fit.lasso <- glmnet(train.mat, table.train$gdp, alpha = 1, lambda=1000, thresh = 1e-12)
cv.lasso <- cv.glmnet(train.mat, table.train$gdp, alpha = 1, thresh = 1e-12)

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
mean((pred.lasso - table.test$gdp)^2)
predict(fit.lasso, s = bestlam.lasso, type = "coefficients")
#3# Decision Tree
library(tree)
tree.table <- tree(gdp~ bachelor+rate123+west+north+mid, data = table.train)
summary(tree.table)
plot(tree.table)
text(tree.table, pretty = 0)
yhat <- predict(tree.table, newdata = table.test)
mean((yhat - table.test$gdp)^2)