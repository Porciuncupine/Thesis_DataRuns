#Final Thesis Code
#installing and updating necessary packages 
install.packages("stats")
install.packages("car")
install.packages("caret")
install.packages("glmnet")
install.packages("sgd")
install.packages("BLR")
install.packages("lars")
install.packages("ggplot2")
install.packages("termplot")
update.packages()

#loading necessary packages 
library(stats)
library(car)
library(caret)
library(glmnet)
library(sgd)
library(BLR)
library(lars)
library(ggplot2) 

#loading,transposing, and naming datasets
CompPer <- t(Thesis.Dataset...Accomplishment.Rate) #Changed Dataset Name
ALI_Caseload_Cases <- t(Thesis.Dataset...ALI) #Changed Dataset and Variable Name
Prod <- t(Thesis.Dataset...Production)
Inequality <- t(Thesis.Dataset...Gini.Coefficient)
View(CompPer)

# Economic Multivariate Regression Model
#CompRate ~ Gini + ALI Cases + Production, for the years '93-'18

ALICases <- as.numeric(ALI_Caseload_Cases[6:26,1])
Gini <- as.numeric(Inequality[6:26, "Gini coefficient"])
Comp_Rate <- as.numeric(CompPer[13:33,1]) 
Production <- as.numeric(Prod[27:47,1])
print(ALICases)

#Bind all Data Sets for Test 5
dataset <- rbind(Comp_Rate, Gini, ALICases, Production)
dataset.df <- data.frame(dataset)
print(dataset.df)


#Simple Regression 
SimpleRegression <- lm(formula=Comp_Rate ~ Gini + ALICases + Production, data=dataset.df)
SimpleRegression
summary(SimpleRegression)

#Logarithmic Regression Dataset
LogALICases <- log(as.numeric(ALI_Caseload_Cases[6:26,1]))
LogGini <- log(as.numeric(Inequality[6:26, "Gini coefficient"]))
LogComp_Rate <- log(as.numeric(CompPer[13:33,1]))
LogProduction <- log(as.numeric(Prod[27:47,1]))
print(LogALICases)

logdataset <- rbind(LogComp_Rate, LogGini, LogALICases, LogProduction)
logdataset.df <- data.frame(logdataset)
print(logdataset.df)


#Logarithmic Regression
LogarithmicRegression <- lm(formula=LogComp_Rate ~ LogGini + LogALICases + LogProduction, data=logdataset.df)
LogarithmicRegression
summary(LogarithmicRegression)

