install.packages("caTools")
library(caTools)
library(rpart)
library(rpart.plot)
library(readr)
global <- read_csv("G:/My Drive/Fall_2021/Data 101/Final project/Possible datasets/global_development.csv")
View(global)


#plots
barplot(global$Year, global$`Data.Health.Life Expectancy at Birth, Total`)
general.ylab = "Life Expectancy at Birth, Total (Years)"
plot(global$`Data.Urban Development.Urban Population Percent`, global$`Data.Health.Life Expectancy at Birth, Total`, xlab = "Percent of Urban Population (%)", ylab = general.ylab)
plot(global$`Data.Health.Fertility Rate`, global$`Data.Health.Life Expectancy at Birth, Total`, xlab = "Fertility rate (Average number of children per woman)", ylab = general.ylab)

# Split data sets
split = sample.split(global$`Data.Urban Development.Urban Population Percent`, SplitRatio = 0.8)
global.train = subset(global, split==TRUE)
global.test = subset(global, split==FALSE)

#Develop a linear model Var 1
lm.lifeExp <- lm(`Data.Health.Life Expectancy at Birth, Total`~`Data.Urban Development.Urban Population Percent`+`Data.Urban Development.Population Density`, data=global.train)
lm.lifeExp.pred <- predict(lm.lifeExp, global.test)
regr.error(lm.lifeExp.pred, global.test$`Data.Health.Life Expectancy at Birth, Total`)
abline(lm.lifeExp, col="red")
#RMSE 7.25

#Rpart model var 1
rpart.lifeExp <- rpart(`Data.Health.Life Expectancy at Birth, Total`~`Data.Urban Development.Urban Population Percent`, data=global.train)
rpart.lifeExp.pred <- predict(rpart.lifeExp, global.test)
regr.error(rpart.lifeExp.pred, global.test$`Data.Health.Life Expectancy at Birth, Total`)
rpart.plot(rpart.lifeExp)
printcp(rpart.lifeExp)
#RMSE 7.1

#Linear model var 2
lm.lifeExp2 <- lm(`Data.Health.Life Expectancy at Birth, Total`~`Data.Health.Fertility Rate`, data=global.train)
lm.lifeExp2.pred <- predict(lm.lifeExp2, global.test)
regr.error(lm.lifeExp2.pred, global.test$`Data.Health.Life Expectancy at Birth, Total`)
abline(lm.lifeExp2, col="green")

#Rpart model var 2
rpart.lifeExp2 <- rpart(`Data.Health.Life Expectancy at Birth, Total`~`Data.Health.Fertility Rate`, data=global.train)
rpart.lifeExp2.pred <- predict(rpart.lifeExp2, global.test)
regr.error(rpart.lifeExp2.pred, global.test$`Data.Health.Life Expectancy at Birth, Total`)
rpart.plot(rpart.lifeExp2)
printcp(rpart.lifeExp2)

#Linear model combining both predictors
lm.lifeExp3 <- lm(`Data.Health.Life Expectancy at Birth, Total`~`Data.Urban Development.Urban Population Percent`+`Data.Health.Fertility Rate`, data=global.train)
lm.lifeExp3.pred <- predict(lm.lifeExp3, global.test)
plot(lm.lifeExp3)
regr.error(lm.lifeExp3.pred, global.test$`Data.Health.Life Expectancy at Birth, Total`)

#Rpart model combinining both predictors
rpart.lifeExp3 <- rpart(`Data.Health.Life Expectancy at Birth, Total`~`Data.Urban Development.Urban Population Percent`+`Data.Health.Fertility Rate`, data=global.train)
rpart.lifeExp3.pred <- predict(rpart.lifeExp3, global.test)
regr.error(rpart.lifeExp3.pred, global.test$`Data.Health.Life Expectancy at Birth, Total`)
rpart.plot(rpart.lifeExp3)
printcp(rpart.lifeExp3)



