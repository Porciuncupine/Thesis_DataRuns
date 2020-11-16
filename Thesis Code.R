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
ARB <- t(Thesis.Dataset...Benficiaries)
Roads <- t(Thesis.Dataset...Roads)
Inequality <- t(Thesis.Dataset...Gini.Coefficient)
Cases_Solved <- t(Thesis.Dataset...Agrarian.Cases.Solved)
CompRate <- t(Thesis.Dataset...Gross.Area.Accomplished)
print(CompRate)

#Subsetting Data For Initial Test 99-00
ARBTest <- ARB[2:3,"Philippines"]
RoadsTest <- Roads[18:19, "Philippines"]
InequalityTest <- Inequality[5:6, "Gini coefficient"]
Cases_SolvedTest <- Cases_Solved[12:13, "Cases Solved"]
print(Cases_SolvedTest)

#Bind all Data Sets 
mydata <- rbind(ARBTest,RoadsTest,InequalityTest,Cases_SolvedTest)
mydata.df <- data.frame(mydata)
print(mydata.df)

#Simple Regression
LReg <- lm(formula= ARBTest~RoadsTest+InequalityTest+Cases_SolvedTest , data=mydata.df)
LReg
summary(LReg)

##########################################################################################

#Subsetting longer datasets 97-17 except Cases
ARBTest2 <- ARB[2:20,"Philippines"]
RoadsTest2 <- Roads[18:36, "Philippines"]
InequalityTest2 <- Inequality[5:23, "Gini coefficient"]
print(InequalityTest2)

#Bind all Data Sets for Test 2
mydata2 <- rbind(ARBTest2,RoadsTest2,InequalityTest2)
mydata2.df <- data.frame(mydata2)
print(mydata2.df)

#Simple Regression Test 2
LReg <- lm(formula= ARBTest2~RoadsTest2+InequalityTest2, data=mydata2.df)
LReg
summary(LReg)
plot(LReg)

#checking pearson correlation
cor(RoadsTest2,InequalityTest2, method="pearson")


####################################################################################

#Using Completion Rate as the Y and Gini,Roads as a determinant, 99-17, Test 3 
ARBTest3 <- ARB[2:20,"Philippines"]
RoadsTest3 <- Roads[18:36, "Philippines"]
InequalityTest3 <- Inequality[5:23, "Gini coefficient"]
CompRate3 <- CompRate[15:33, "Philippines"]
print(ARBTest3)


#Bind all Data Sets for Test 3
mydata3 <- rbind(RoadsTest3,InequalityTest3,CompRate3,ARBTest3)
mydata3.df <- data.frame(mydata3)
print(mydata3.df)

#Simple Regression Test 3
LReg <- lm(formula= CompRate3~RoadsTest3+InequalityTest3+ARBTest3, data=mydata3.df)
LReg
summary(LReg)

#checking pearson correlation
cor(ARBTest3,RoadsTest3, method="pearson")

