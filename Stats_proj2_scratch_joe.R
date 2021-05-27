##

Reds <- read.csv("wineQualityReds.csv")
Whites <- read.csv("wineQualityWhites.csv")
Reds$type <- "red"
Whites$type <- "white"

full_df <- rbind(Reds,Whites)
#full_df$X <- NULL
View(full_df)


full_df$good <- ifelse(full_df$quality >= 8,1,0)
full_df$good <- as.factor(full_df$good )

sum(full_df$good == 1)/nrow(full_df)


par(mfrow=c(2,3))
boxplot(citric.acid~quality, data=full_df, main="Boxplot 1")
boxplot(alcohol~quality, data=full_df, main="Boxplot 2")
boxplot(pH~quality, data=full_df, main="Boxplot 3")
boxplot(volatile.acidity~quality, data=full_df, main="Boxplot 4")
boxplot(fixed.acidity~quality, data=full_df, main="Boxplot 5")
boxplot(density~quality, data=full_df, main="Boxplot 6")

par(mfrow=c(2,3))
boxplot(residual.sugar~quality, data=full_df, main="Boxplot 7")
boxplot(free.sulfur.dioxide~quality, data=full_df, main="Boxplot 8")
boxplot(total.sulfur.dioxide~quality, data=full_df, main="Boxplot 9")
boxplot(sulphates~quality, data=full_df, main="Boxplot 10")
boxplot(chlorides~quality, data=full_df, main="Boxplot 11")

cor(full_df)

full_df$quality <- NULL


set.seed(8)
sample<-sample.int(nrow(full_df), floor(.50*nrow(full_df)), replace = F)
train<-full_df[sample, ]
test<-full_df[-sample, ]

model <- glm(good~.,family = binomial,data = train)

preds<-predict(model,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$good)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

plot(roc_result, main="ROC Curve for Quality")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values


full_df1<- full_df
full_df1$type <- NULL

library(dplyr)
df2 <- mutate_all(full_df1, function(x) as.numeric(as.character(x)))

cor(df2)


#reduced model droping citric.acid and total.sulfur.dioxide
model2 <-  glm(good~ fixed.acidity + volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide + density + pH + sulphates + alcohol + type,family = binomial,data = train)
summary(model2)

1-pchisq(model2$deviance-model$deviance, 2)
#model2$deviance - model$deviance

#drop free.sulfur.dioxide
model3 <-  glm(good~ fixed.acidity + volatile.acidity + residual.sugar + chlorides + density + pH + sulphates + alcohol + type,family = binomial,data = train)
#both test are insignificant
1-pchisq(model3$deviance-model2$deviance, 1)
1-pchisq(model3$deviance-model$deviance, 3)

summary(model3)
summary(model2)

#we have collinearity so let's drop density
#we found that density is a function of alcohol and sugar
#also density has a huge coefficient with large std error
model4 <- glm(good~ fixed.acidity + volatile.acidity + residual.sugar + chlorides + pH + sulphates + alcohol + type,family = binomial,data = train)
summary(model4)



#actually let's try dropping density first
model_minus_density <- glm(good~.-density, family = binomial,data = train)
summary(model_minus_density)

#let's drop citric.acid and type
model5 <-  glm(good~ fixed.acidity + volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol ,family = binomial,data = train)
summary(model5)

1-pchisq(model5$deviance-model_minus_density$deviance,2)

#let's drop total.sulfur.dioxide
model6 <-  glm(good~ fixed.acidity + volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide + pH + sulphates + alcohol ,family = binomial,data = train)
summary(model6)

#we can't drop total.sulfur.dioxide
1-pchisq(model6$deviance-model5$deviance,1)

#try dropping free.sulfur.dioxide 
model7 <-  glm(good~ fixed.acidity + volatile.acidity + residual.sugar + chlorides + total.sulfur.dioxide + pH + sulphates + alcohol ,family = binomial,data = train)
1-pchisq(model7$deviance-model5$deviance,1)


#drop fixed.acidity and total.sulfur.dioxide
model8 <-  glm(good~ volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide  + pH + sulphates + alcohol ,family = binomial,data = train)
#significant so we can't drop
1-pchisq(model8$deviance-model5$deviance,2)




#Removing Density at the beginning of the process

#p-value method
model_candidate1 <- glm(good~ fixed.acidity + volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol ,family = binomial,data = train)
#forward/both 
model_candidate2 <- glm(good~  volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide  + sulphates + alcohol ,family = binomial,data = train)
#backward
model_candidate3 <- glm(good~ fixed.acidity + volatile.acidity + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol + type,family = binomial,data = train)


model_candidates_list <- c(model_candidate1,model_candidate2,model_candidate3)

#==============================Model Candidates============================================================

##predicted survival rate for testing data based on training data
#print(summary(i))
preds<-predict(model_candidate1,newdata=test, type="response")
  
##produce the numbers associated with classification table
rates<-prediction(preds, test$good)
  
##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
  
##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Titanic")
lines(x = c(0,1), y = c(0,1), col="red")
  
##compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values

summary(model_candidate1)
summary(model_candidate2)
summary(model_candidate3)


preds<-predict(model_candidate2,newdata=test, type="response")
  
##produce the numbers associated with classification table
rates<-prediction(preds, test$good)
  
##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
  
##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Titanic")
lines(x = c(0,1), y = c(0,1), col="red")
  
##compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values


preds<-predict(model_candidate3,newdata=test, type="response")

##produce the numbers associated with classification table
rates<-prediction(preds, test$good)

##store the true positive and false postive rates
roc_result<-performance(rates,measure="tpr", x.measure="fpr")

##plot ROC curve and overlay the diagonal line for random guessing
plot(roc_result, main="ROC Curve for Titanic")
lines(x = c(0,1), y = c(0,1), col="red")

##compute the AUC
auc<-performance(rates, measure = "auc")
auc@y.values










