#Efe Umukoro - STAT511 Project#2 for Professor Millie (Spring 2021)

#Input data from local drive within folder "Project2"
sales=read.table("File Path", header=FALSE)
View(sales)

#Dataset: APPENC07.txt(Real Estate Sales)
#The  city  tax  assessor  was  interested  in  predicting  residential  home  sales  
#prices  in  a  midwestern city as a function of various characteristics of the home 
#and surrounding  property. Data on 522 transactions were obtained for home sales 
#during the year 2002.

#Variables of Analysis: Response variable(Y):
#Sales price of a residence (in dollars), Column 2.
#Expl variable (X 1): Number of bedrooms, Column 4.
#Expl variable (X 2):Number of bathrooms, Column 5.
#Expl variable (X 3): Garage size (num of cars that garage can hold), Column 7.

price=sales$V2
bedroom=sales$V4
bathroom=sales$V5
garage=sales$V7

#Sample size n = 522

#MLR (multiple linear regression) model w/ multiple X variables
sales.lm=lm(price ~ bedroom + bathroom + garage)
summary(sales.lm)

#Calculating 95% confidence/prediction intervals
#confint(sales.lm, level = 0.95)
#predict(sales.lm, interval = "confidence")
#predict(sales.lm, interval = "prediction")

#From lesson slides: CI and PI for new X values (out-of-sample)
#First create a dataframe of new X values
# Section 2 - Set bedroom to 3, bathroom to 3, and garage to 2
new.data = data.frame(bedroom=c(3), bathroom=c(3), garage=c(2))
#Point estimate
predict(sales.lm, new.data)
#CI
predict(sales.lm, new.data, interval = "confidence")
#PI
predict(sales.lm, new.data, interval = "prediction")

#*********************Part 3 - Hypothesis Testing****************************
#Anova
anova(sales.lm)

#Part 3B Create Joint MLR Models

#lm.bathroom = lm(price ~ bathroom)
#lm.garage = lm(price ~ garage)
#lm.bathroom.garage = lm(price ~ bathroom + garage)
#anova(lm.bathroom, lm.bathroom.garage)
#anova(lm.garage, lm.bathroom.garage)


lm.bedroom = lm(price ~ bedroom)
lm.full = lm(price ~ bedroom + bathroom + garage)
anova(lm.bedroom, lm.full)

#*********************Part 4 - Multicollinearity*****************************
#Scatterplot and Correlation Matrix of the three predictors
plot(data.frame(bedroom, bathroom, garage))
cor(data.frame(bedroom, bathroom, garage))

#**Removal of the Bedroom Predictor
#*Variables
price1 = sales$V2
bedrooms1 = sales$V4
bathrooms1 = sales$V5
garage1 = sales$V7

#*New MLR model removed bedrooms
new.lm = lm(price1 ~ bedrooms1 + garage1)
anova(new.lm)

