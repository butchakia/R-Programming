#1
library(readxl)
breast_cancer_data <- read.csv("C:/Users/whitl/OneDrive/Desktop/R Files/breast_cancer_data.csv")
View(breast_cancer_data)

library(ggplot2)

#2A
breast_cancer_data$diagnosis <- factor(breast_cancer_data$diagnosis, levels = c("B", "M"))
function (diagnosis, area_mean, data=breast_cancer_data){
   BoxplotPredictorOnTarget <- glm(diagnosis~area_mean, data=breast_cancer_data)
  return(BoxplotPredictorOnTarget)
}
ggplot(data=BoxplotPredictorOnTarget, aes(x=diagnosis, y=area_mean))+
  geom_boxplot(notch=TRUE)

#2B
breast_cancer_data$diagnosis <- factor(breast_cancer_data$diagnosis, levels = c("B", "M"))
function (diagnosis, area_se, data=breast_cancer_data){
  BoxplotPredictorOnTarget <- glm(diagnosis~breast_cancer_data$area_se, data=breast_cancer_data)
  return(BoxplotPredictorOnTarget)
}
ggplot(data=BoxplotPredictorOnTarget, aes(x=diagnosis, y=breast_cancer_data$area_se))+
  geom_boxplot(notch=TRUE)

#2C
breast_cancer_data$diagnosis <- factor(breast_cancer_data$diagnosis, levels = c("B", "M"))
function (diagnosis, texture_mean, data=breast_cancer_data){
  BoxplotPredictorOnTarget <- glm(diagnosis~breast_cancer_data$texture_mean, data=breast_cancer_data)
  return(BoxplotPredictorOnTarget)
}
ggplot(data=BoxplotPredictorOnTarget, aes(x=diagnosis, y=breast_cancer_data$texture_mean))+
  geom_boxplot(notch=TRUE)

#3.A
breast_cancer_data$diagnosis <- as.factor(breast_cancer_data$diagnosis)
glm.a <- glm(diagnosis~area_mean, family=binomial, data=breast_cancer_data)
library(pscl)
pR2(glm.a)
predicted.prob.a <- predict(glm.a, breast_cancer_data, type = "response")
threshold <- .5
breast_cancer_data$predicted.diagnosis.a <- ifelse(predicted.prob.a >= threshold, "M","B")
my.accuracy.a <- mean(breast_cancer_data$predicted.diagnosis.a == breast_cancer_data$diagnosis)
message("The accuracy is ", my.accuracy.a)

#3.B
glm.b <- glm(diagnosis~area_mean+area_se, family=binomial, data=breast_cancer_data)
pR2(glm.b)
predicted.prob.b <- predict(glm.b, breast_cancer_data, type = "response")
predicted.prob.b[1:5]
threshold <- .5
breast_cancer_data$predicted.diagnosis.b <- ifelse(predicted.prob.b >= threshold,"M","B")
my.accuracy.b <- mean(breast_cancer_data$predicted.diagnosis.b == breast_cancer_data$diagnosis)
message("The accuracy is ", my.accuracy.b)

#3.C
glm.c <- glm(diagnosis~area_mean+area_se+texture_mean, family=binomial, data=breast_cancer_data)
pR2(glm.c)
predicted.prob.c <- predict(glm.c, breast_cancer_data, type = "response")
predicted.prob.c[1:5]
threshold <- .5
breast_cancer_data$predicted.diagnosis.c <- ifelse(predicted.prob.c >= threshold,"M","B")
my.accuracy.c <- mean(breast_cancer_data$predicted.diagnosis.c == breast_cancer_data$diagnosis)
message("The accuracy is ", my.accuracy.c)

#3.D
glm.d <- glm(diagnosis~area_mean+area_se+texture_mean+concavity_worst, family=binomial, data=breast_cancer_data)
pR2(glm.d)
predicted.prob.d <- predict(glm.d, breast_cancer_data, type = "response")
predicted.prob.d[1:5]
threshold <- .5
breast_cancer_data$predicted.diagnosis.d <- ifelse(predicted.prob.d >= threshold,"M","B")
my.accuracy.d <- mean(breast_cancer_data$predicted.diagnosis.d == breast_cancer_data$diagnosis)
message("The accuracy is ", my.accuracy.d)

#3.E
glm.e <- glm(diagnosis~area_mean+area_se+texture_mean+concavity_worst+concavity_mean, family=binomial, data=breast_cancer_data)
pR2(glm.e)
predicted.prob.e <- predict(glm.c, breast_cancer_data, type = "response")
predicted.prob.e[1:5]
threshold <- .5
breast_cancer_data$predicted.diagnosis.e <- ifelse(predicted.prob.e >= threshold,"M","B")
my.accuracy.e <- mean(breast_cancer_data$predicted.diagnosis.e == breast_cancer_data$diagnosis)
message("The accuracy is ", my.accuracy.e)
