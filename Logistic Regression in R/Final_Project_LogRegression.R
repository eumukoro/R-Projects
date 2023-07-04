#Efe Umukoro
#Logistic Regression Project
#12/16/2021


library(haven)
iv <- read_dta("Desktop/iv.dta")
View(iv)
iv



#overview of data

summary(iv)
summary(age)
summary(alcohol)
summary(tobacco)
str(iv)
names(iv)


#Variables (all variables utilized in the Regression Analysis)

case = iv$case
case

age = iv$age
age

age_group = iv$agegp
age_group

tobacco = iv$tob
tobacco

tobacco_group = iv$tobgp
tobacco_group

alcohol = iv$alc
alcohol

alcohol_group = iv$alcgp
alcohol_group



#Conduct a Univariate analysis study of each variable

age.model = glm(case ~ age, family = binomial)
age.model
summary(age.model)
#It is clear that the p-value is smaller than 0.025, include the variable age in the model.

tobacco.model = glm(case ~ tobacco, family = binomial)
tobacco.model
summary(tobacco.model)
#It is clear that the p-value is smaller than 0.025, include the variable tobacco in the model.

alcohol.model = glm(case ~ alcohol, family = binomial)
alcohol.model
summary(alcohol.model)
#it is clear that the p-value is smaller than 0.025, include the variable alcohol in the model.

#Create a Model

model.iv = glm(case ~ age + tobacco + alcohol, family = binomial)
model.iv
summary(model.iv)

#We seee that all of the variables have p-values that are smaller than 0.025.  It may
#be the case that all three of the variables are important in establishing whether
#a patient may or may not have esophogeal cancer. 

#Interactions of amongst variables

model.ivint1 = glm(case ~ age + tobacco + alcohol + age:tobacco, family = binomial) 
model.ivint1
summary(model.ivint1)
#interaction not significant

model.ivint2 = glm(case ~ age + tobacco + alcohol + age:alcohol, family = binomial)
model.ivint2
summary(model.ivint2)
#interaction not significant

model.ivint3 = glm(case ~ age + tobacco + alcohol + tobacco:alcohol, family = binomial)
model.ivint3
summary(model.ivint3)
#interaction not significant

# Determine the final model
iv.finalmodel = glm(case ~ age + tobacco + alcohol , family = binomial, data = iv)
iv.finalmodel
summary(iv.finalmodel)

y = -7.572920 + 0.072289*age + 0.038803*tobacco + 0.026490*alcohol
y


#Assess model fit by conducting the pearson chi-square test and the Holsem test

#Assessing model fit using the pearson chi-square test
#Perform the pearson test 

#Assess model fit usin the pearson chi square test. 
dr1 = residuals(iv.finalmodel)
dr1

x1 = sum(dr1^2)
x1

pr1 = residuals(iv.finalmodel,"pearson")
pr1

y1 = sum(pr1^2)
y1

prob1 = 1 - pchisq(718.281, 851.8147) 
prob1





# Perform a hypothesis test

#Assess the model fit using the hoselm test

install.packages("ResourceSelection")
library("ResourceSelection")
library("broom")

?hoslem.test

y = sum(pr1^2)
y

ivfinalmodel = glm(case ~ age + tobacco + alcohol , family = binomial)
ivfinalmodel

z = hoslem.test(ivfinalmodel$y, fitted(ivfinalmodel))
z

# residual analysis
install.packages("broom")
library(broom)

plot(ivfinalmodel)
plot(ivfinalmodel, which = 4, id.n = 3)


#study by removing 

ivfinalmodel = glm(case ~ age + tobacco + alcohol , family = binomial)
ivfinalmodel
summary(ivfinalmodel)

ivfinalmodel_noage = glm(case ~ tobacco + alcohol , family = binomial)
ivfinalmodel_noage
summary(ivfinalmodel_noage)

a = logLik(ivfinalmodel)
b = logLik(ivfinalmodel_noage)

G = -2*(b-(a))
G
1 - pchisq(G, 1)

summary(ivfinalmodel)


