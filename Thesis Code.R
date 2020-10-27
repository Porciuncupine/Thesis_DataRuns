##Thesis Data Set Initial Run
##Incomplete Data Set Runs 

#installing and updating necessary packages 
  install.packages("stats")
  install.packages("car")
  install.paclages("caret")
  install.paclages("glmnet")
  install.paclages("sgd")
  install.paclages("BLR")
  install.paclages("lars")
  install.packages("ggplot2")
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
ARB <- t(Thesis.Dataset...Benficiaries)
Roads <- t(Thesis.Dataset...Roads)
Inequality <- t(Thesis.Dataset...Gini.Coefficient)
Cases_Solved <- t(Thesis.Dataset...Agrarian.Cases.Solved)

#Subsetting Data For Initial Test 99-00
ARBTest <- ARB[2:3,"Philippines"]
RoadsTest <- Roads[18:19, "Philippines"]
InequalityTest <- Inequality[5:6, "Gini coefficient"]
Cases_SolvedTest <- Cases_Solved[12:13, "Cases Solved"]

#Bind all Data Sets 
mydata <- cbind(ARBTest,RoadsTest,InequalityTest,Cases_SolvedTest)
print(mydata)

#Simple Regression
lm(formula=X1999~X2000, data=mydata)



