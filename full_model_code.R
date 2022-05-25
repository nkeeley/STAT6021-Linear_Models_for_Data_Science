data<-read.csv("clean_diamond_data.csv", header=TRUE, )
library(tidyverse)
library(faraway)
attach(data)
#Cleaning the data



predictors_full_model <- cbind(carat,clarity,color,cut,price)
predictor_matrix<- data_frame(predictors_full_model)

full_model <- lm(price ~ carat + clarity + color + cut) 
reduced_model <- lm(price ~ carat )
anova(full_model, reduced_model)

plot(price~carat, xlab = "Carat", ylab = "Price (dollar)", main = "Price (All Data Points)")

#Checking the data
summary(full_model) #according to the P value for the T test, we reject the Null, all predictors is signfignant holding all other predictors constant.
cor(predictors_full_model) #No correlative affects, between prdictors, no multicolinearity.
vif(full_model) # Cut and clarity are highly correlated according to the VIFs.

reduced_model2<-lm(price~carat+color+cut)

#Testing full vs reduced model
anova(reduced_model2,full_model) #Reject the Null, cannot discard the variables in question.


#plot
#Full Model Linearity Assumption Checks
dev_off()
plot(full_model$fitted.values,full_model$residuals,main="Residual Plot", xlab = "Fitted Values", ylab = "Residuals")
abline(h=0,col="red")
acf(full_model$residuals, main="Auto Correlation Function")
abline(h = 0 ,col = 'red')
qqnorm(full_model$residuals)
qqline(full_model$residuals)



library(ggplot2)
ggplot(data, aes(x = as.factor(cut), y = price) )+ geom_point() +geom_line() 
#Good
par(mfrow=c(1,2))
good <- filter(data, cut == 'Good')
plot(good$carat, good$price)

#VeryGood
verygood <- filter(data, cut == 'Very Good')
plot(verygood$carat,verygood$price)



#Transformations On full Population parameter
library(MASS) ##boxcox function in MASS package.
result <- lm(full_model)
boxcox(full_model)
price_log = log(price)
log_carat = log(data$carat)



result_log <-lm(price_log~carat + color + cut + clarity)
summary(result_log)

cor(result_log)
plot(result_log$fitted.values,result_log$residuals,main="Residual plot")
abline(h=0, col="red")
view(result_log)
plot(log_carat, price_log)

log_transformations <- cbind(price_log,log_carat)
new_df <- cbind(data,log_transformations)
detach(data)
attach(new_df)

new_model <- lm(price_log~log_carat + cut + clarity + color)

new_reduced <- lm(price_log~log_carat)
summary(new_model)


## Playing with regsubsets
test<-regsubsets(new_model)
anova(new_reduced,new_model)

plot(price_log,log_carat, xlab = "Carat (log)", ylab = "Price (dollar, log)", main = "Price vs. Carat Size (All Data Points, T2)")


plot(new_model$fitted.values,new_model$residuals,xlab = "Fitted Values", ylab = "Residuals", main="Residual plot")
abline(h=0,col="red")
acf(new_model$residuals, main="Auto Correlation Function Plot")
abline(h = 0 ,col = 'red')
qqnorm(new_model$residuals)
qqline(new_model$residuals)

#Ideas Further work: Playing with reduced model
#check for variance across classes.
#Bin data

