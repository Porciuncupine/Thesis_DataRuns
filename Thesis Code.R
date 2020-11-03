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
  
##############################################################################################
  
  
#loading,transposing, and naming datasets
ARB <- t(Thesis.Dataset...Benficiaries)
Roads <- t(Thesis.Dataset...Roads)
Inequality <- t(Thesis.Dataset...Gini.Coefficient)
Cases_Solved <- t(Thesis.Dataset...Adjudication.Cases.Resolved.per.Year)
CompRate <- t(Thesis.Dataset...Gross.Area.Accomplished)
AllImports <- t(Thesis_Dataset_All_Imports)
AllExports <- t(Thesis_Dataset_All_Exports_)
CerealImports <- t(`Thesis.Dataset...Rice.and.Corn.Imports.Value.in.Nominal.US$`)
CompPer <- t(Thesis.Dataset...Accomplishment.Rate)
ALICases <- t(Thesis.Dataset...ALI)
Pending <- t(Thesis.Dataset...Pending_Caseload.Agrarian.Law.IMP)
Prod <- t(Thesis.Dataset...Production)
Yield <- t(Thesis.Dataset...Yield)
print(Yield)

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
plot(LReg)

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


#checking pearson correlation
cor(RoadsTest2,InequalityTest2, method="pearson")


####################################################################################

#Using Completion Rate as the Y and Gini,Roads as a determinant, 99-17, Test 3 
ARBTest3 <- ARB[2:20,"Philippines"]
RoadsTest3 <- Roads[18:36, "Philippines"]
InequalityTest3 <- Inequality[5:23, "Gini coefficient"]
CompRateTest3 <- CompRate[15:33, "Philippines"]
print(ARBTest3)

#Bind all Data Sets for Test 3
mydata3 <- rbind(RoadsTest3,InequalityTest3,CompRateTest3,ARBTest3)
mydata3.df <- data.frame(mydata3)
print(mydata3.df)

#Simple Regression Test 3
LReg <- lm(formula= ARBTest3~RoadsTest3+InequalityTest3, data=mydata3.df)
LReg
summary(LReg)
plot(LReg)

#checking pearson correlation
cor(CompRateTest3,RoadsTest3, method="pearson")


########################################################################################

#ARBs ~ SOLVED + IMPORTS + EXPORTS + ROADS, 99-18

ARBTest00 <- as.numeric(ARB[2:21,"Philippines"])
RoadsTest00 <- as.numeric(Roads[18:37, "Philippines"])
AllImports00 <- as.numeric(AllImports[21:40,1])
AllExports00 <- as.numeric(AllExports[21:40,1])
Cases_Solved00 <- as.numeric(Cases_Solved[12:31,1])
InequalityTest00 <- Inequality[5:24, "Gini coefficient"]
print(InequalityTest00)

#Bind all Data Sets for Test Log
mydata4 <- rbind(ARBTest00,RoadsTest00,AllImports00,AllExports00,Cases_Solved00,InequalityTest00)
mydata4.df <- data.frame(mydata4)
print(mydata4.df)

#Simple Regression Test 3
LReg <- lm(formula= ARBTest00~RoadsTest00+AllExports00+AllImports00+Cases_Solved00, data=mydata4.df)
LReg
summary(LReg)


#log 
LogARBTest00 <- log(as.numeric(ARB[2:21,"Philippines"]))
LogRoadsTest00 <- log(as.numeric(Roads[18:37, "Philippines"]))
LogAllImports00 <- log(as.numeric(AllImports[21:40,1]))
LogAllExports00 <- log(as.numeric(AllExports[21:40,1]))
LogCases_Solved00 <- log(as.numeric(Cases_Solved[12:31,1]))
LogInequalityTest00 <- log(Inequality[5:24, "Gini coefficient"])
print(LogRoadsTest00)

#Bind all Data Sets for Test Log
mydata4 <- rbind(LogRoadsTest00,LogAllImports00,LogAllExports00,LogARBTest00,LogCases_Solved00,LogInequalityTest00)
mydata4.df <- data.frame(mydata4)
print(mydata4.df)

#Simple Regression Test 3
LogReg <- lm(formula= LogARBTest00~LogRoadsTest00+LogAllImports00+LogAllExports00+LogCases_Solved00, data=mydata4.df)
LogReg
summary(LogReg)


###############################################################
#CompRate ~ ALI Cases + Imports + Exports + Roads + Gini, '99-'18

ARBTest00 <- as.numeric(ARB[2:21,"Philippines"])
RoadsTest00 <- as.numeric(Roads[18:37, "Philippines"])
AllImports00 <- as.numeric(AllImports[21:40,1])
AllExports00 <- as.numeric(AllExports[21:40,1])
ALICases00 <- as.numeric(ALICases[7:26,1])
InequalityTest00 <- Inequality[5:24, "Gini coefficient"]
CompPer00 <- as.numeric(CompPer[15:34,1])
Pending00 <- as.numeric(Pending[7:26,1])
Prod00 <- as.numeric(Prod[28:47,1])
Yield00 <- as.numeric(Yield[28:47,1]) 
print(Yield00)



#Bind all Data Sets for Test Log
mydata4 <- rbind(Prod00,ARBTest00,RoadsTest00,AllImports00,AllExports00,ALICases00,InequalityTest00, CompPer00)
mydata4.df <- data.frame(mydata4)
print(mydata4.df)

#Simple Regression Test 3
LReg <- lm(formula= CompPer00~RoadsTest00+AllExports00+AllImports00+ALICases00, data=mydata4.df)
LReg
summary(LReg)


#log 
LogARBTest00 <- log(as.numeric(ARB[2:21,"Philippines"]))
LogRoadsTest00 <- log(as.numeric(Roads[18:37, "Philippines"]))
LogAllImports00 <- log(as.numeric(AllImports[21:40,1]))
LogAllExports00 <- log(as.numeric(AllExports[21:40,1]))
LogALICases00 <- log(as.numeric(ALICases[7:26,1]))
LogInequalityTest00 <- log(Inequality[5:24, "Gini coefficient"])
LogCompPer00 <- log(as.numeric(CompPer[15:34,1]))
LogPending00 <- log(as.numeric(Pending[7:26,1]))
LogProd00 <- log(as.numeric(Prod[28:47,1]))
LogYield00 <- log(as.numeric(Yield[28:47,1]))
print(LogYield00)

#Bind all Data Sets for Test Log
mydata4 <- rbind(LogARBTest00,LogRoadsTest00,LogAllImports00,LogAllExports00,LogARBTest00,LogALICases00,LogInequalityTest00,LogCompPer00,LogProd00,LogYield00)
mydata4.df <- data.frame(mydata4)
print(mydata4.df)

#Simple Regression Test 3
LogReg <- lm(formula= LogCompRate00~LogRoadsTest00+LogAllImports00+LogAllExports00+LogALICases00+LogInequalityTest00, data=mydata4.df)
LogReg
summary(LogReg)

#Simple Regression Test 3
LogReg <- lm(formula= LogCompPer00~LogALICases00+LogInequalityTest00+LogProd00+LogAllImports00, data=mydata4.df)
LogReg
summary(LogReg)
cor(LogProd00,LogCompPer00, method="pearson")
cor(LogALICases00,LogCompPer00, method="pearson")
cor(LogInequalityTest00, LogCompPer00,method="pearson")
cor(LogAllImports00, LogCompPer00, method="pearson")
