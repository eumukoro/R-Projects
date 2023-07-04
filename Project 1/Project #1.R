#Efe Umukoro | Project #1
#Part 1: Interpretation and Parameter Inference
#Response and Explanatory Variable

View(SENIC)
x = SENIC$INFRISK
y = SENIC$LOS
x3 = SENIC$AGE

#Linear Model
model = lm(y~x)
model

#Draw a Scatterplot and regression
plot(x,y)
abline(model)

#Regression Coefficient
summary(model)

#Confidence Interval
confint(model, level = 0.99)
confint(model)

#Part 2: Point and Interval Estimation
#Fitted Value 

xl=5
risk = 6.3368 + 0.7604*x1
risk

#Confidence Interval for infection risk at 5%
x2 = data.frame(x=c(5))

predict(model, x2)
predict(model, x2, interval = "confidence")


#Prediction Interval 
predict(model, x2, interval = "prediction")

#Part 3: Diagnostics 
#Normality and Equal Variance Test

#Plot of the linear model 

plot(x, model$residuals, col="green")

library(MASS)
#Residual Plot of the linear model
stud.resid = studres(model)


#********studentized residual plot vs. predictor
plot(x,stud.resid, col="green")

#*******Box Plot of the residuals of the linear model 
boxplot(model$resid, col="green")


#********Histogram of the residuals of the linear model 
hist(model$resid, col="green")


#Normality Test
library(nortest)
shapiro.test(stud.resid)
sf.test(stud.resid)
ad.test(stud.resid)



#Equal Variance Assumptions
library(car)
x.med =  median(x)
x.group = ifelse(x<x.med, "Group1", "Group2")
leveneTest(model$residuals, x.group)

library(lmtest)
bptest(model)
bptest(model, studentize= FALSE)

library(olsrr)
ols_pure_error_anova(model)

#Check for omitted Predictor
plot(x,y)
plot(x3, y)
plot(x3,model$residuals)

age.lm = lm(y~x + x3)
age.lm

summary(age.lm)











