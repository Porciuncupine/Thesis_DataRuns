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
Inequality <- t(Thesis.Dataset...Gini.Coefficient)
CompPer <- t(Thesis.Dataset...Accomplishment.Rate) #Changed Dataset Name
ALI_Caseload_Cases <- t(Thesis.Dataset...ALI) #Changed Dataset and Variable Name
Prod <- t(Thesis.Dataset...Production)

####################################################################
#Yearly Data Test to Check if there are difference between decades #
####################################################################
#CompPer ~ ALI Cases + Inequality + Production, '93-'97

# '93-'97
ALICases9397 <- as.numeric(ALI_Caseload_Cases[1:5,1]) #Changed Name
InequalityTest9397 <- as.numeric(Inequality[1:5, "Gini coefficient"])
CompPer9397 <- as.numeric(CompPer[9:13,"Philippines"]) 
Prod9397 <- as.numeric(Prod[22:26,"Total"])
print(CompPer9397)


#Bind all Data Sets for '93-'97
mydata9397 <- rbind(ALICases9397,InequalityTest9397, CompPer9397, Prod9397)
mydata9397.df <- data.frame(mydata9397)
print(mydata9397.df)

#Simple Regression Test '93-'97
LReg <- lm(formula= CompPer9397~Prod9397+ALICases9397+InequalityTest9397, data=mydata9397.df)
LReg
summary(LReg)


# LogCompPer ~ LogALI Cases + LogInequality + LogProduction, '93-'97
###################################################################################

#Logging Datasets
LogALICases9397 <- log(as.numeric(ALI_Caseload_Cases[1:5,1]))
LogInequalityTest9397 <- log(as.numeric(Inequality[1:5, "Gini coefficient"]))
LogCompPer9397 <- log(as.numeric(CompPer[9:13, 1])) 
LogProd9397 <- log(as.numeric(Prod[22:26,"Total"]))
print(LogCompPer9397)


#Bind all Data Sets for '93-'97
mydata9397_log <- rbind(LogALICases9397, LogALICases9397, LogCompPer9397, LogProd9397)
mydata9397_log.df <- data.frame(mydata9397_log)
print(mydata9397_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPer9397~LogProd9397+LogALICases9397+LogInequalityTest9397, data=mydata9397_log.df)
LogReg
summary(LogReg)

# Multicollinearity Test
car::vif(LogReg)

#Heteroscedasticity Test
lmtest::bptest(LogReg)
car::ncvTest(LogReg)

#Autocorrelation Test
checkresiduals(LogReg, lag=1)


# Checking for correlations
cor(LogProd9397,LogCompPer9397, method="pearson")
cor(LogALICases9397,LogCompPer9397, method="pearson")
cor(LogInequalityTest9397, LogCompPer9397,method="pearson")


####################################################################
#Yearly Data Test to Check if there are difference between decades #
####################################################################
#CompPer ~ ALI Cases + Inequality + Production, '98-'02

# '98-'02
ALICases9802 <- as.numeric(ALI_Caseload_Cases[6:10,1]) #Changed Name
InequalityTest9802 <- as.numeric(Inequality[6:10, "Gini coefficient"])
CompPer9802 <- as.numeric(CompPer[14:18,"Philippines"]) 
Prod9802 <- as.numeric(Prod[27:31,"Total"])
print(InequalityTest9802)


#Bind all Data Sets for '98-'02
mydata9802 <- rbind(ALICases9802,InequalityTest9802, CompPer9802, Prod9802)
mydata9802.df <- data.frame(mydata9802)
print(mydata9802.df)

#Simple Regression '98-'02
LReg <- lm(formula= CompPer9802~Prod9397+ALICases9802+InequalityTest9802, data=mydata9802.df)
LReg
summary(LReg)


# LogCompPer ~ LogALI Cases + LogInequality + LogProduction, '98-'02
###################################################################################

#Logging Datasets
LogALICases9802 <- log(as.numeric(ALI_Caseload_Cases[6:10,1]))
LogInequalityTest9802 <- log(as.numeric(Inequality[6:10, "Gini coefficient"]))
LogCompPer9802 <- log(as.numeric(CompPer[14:18, 1])) 
LogProd9802 <- log(as.numeric(Prod[27:31,"Total"]))
print(LogCompPer9802)


#Bind all Data Sets for Log
mydata9802_log <- rbind(LogALICases9802, LogALICases9802, LogCompPer9802, LogProd9802)
mydata9802_log.df <- data.frame(mydata9802_log)
print(mydata9802_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPer9802~LogProd9802+LogALICases9802+LogInequalityTest9802, data=mydata9802_log.df)
LogReg
summary(LogReg)

# Multicollinearity Test
car::vif(LogReg)

#Heteroscedasticity Test
lmtest::bptest(LogReg)
car::ncvTest(LogReg)

#Autocorrelation Test
checkresiduals(LogReg, lag=1)

# Checking for correlations
cor(LogProd9397,LogCompPer9397, method="pearson")
cor(LogALICases9397,LogCompPer9397, method="pearson")
cor(LogInequalityTest9397, LogCompPer9397,method="pearson")

####################################################################
#Yearly Data Test to Check if there are difference between decades #
####################################################################
#CompPer ~ ALI Cases + Inequality + Production, '03-'07

# '03-'07
ALICases0307 <- as.numeric(ALI_Caseload_Cases[11:15,1]) #Changed Name
InequalityTest0307 <- as.numeric(Inequality[11:15, "Gini coefficient"])
CompPer0307 <- as.numeric(CompPer[19:23,"Philippines"]) 
Prod0307 <- as.numeric(Prod[32:36,"Total"])
print(InequalityTest0307)


#Bind all Data Sets for '03-'07
mydata0307 <- rbind(ALICases0307,InequalityTest0307, CompPer0307, Prod0307)
mydata0307.df <- data.frame(mydata0307)
print(mydata0307.df)

#Simple Regression '03-'07
LReg <- lm(formula= CompPer0307~Prod0307+ALICases0307+InequalityTest0307, data=mydata0307.df)
LReg
summary(LReg)


# LogCompPer ~ LogALI Cases + LogInequality + LogProduction, '03-'07
###################################################################################

#Logging Datasets
LogALICases0307 <- log(as.numeric(ALI_Caseload_Cases[11:15,1]))
LogInequalityTest0307 <- log(as.numeric(Inequality[11:15, "Gini coefficient"]))
LogCompPer0307 <- log(as.numeric(CompPer[19:23, 1])) 
LogProd0307 <- log(as.numeric(Prod[32:36,"Total"]))
print(LogCompPer0307)


#Bind all Data Sets for Log
mydata0307_log <- rbind(LogALICases0307, LogALICases0307, LogCompPer0307, LogProd0307)
mydata0307_log.df <- data.frame(mydata0307_log)
print(mydata0307_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPer0307~LogProd0307+LogALICases0307+LogInequalityTest0307, data=mydata0307_log.df)
LogReg
summary(LogReg)

# Multicollinearity Test
car::vif(LogReg)

#Heteroscedasticity Test
lmtest::bptest(LogReg)
car::ncvTest(LogReg)

#Autocorrelation Test
checkresiduals(LogReg, lag=1)

# Checking for correlations
cor(LogProd0307,LogCompPer0307, method="pearson")
cor(LogALICases0307,LogCompPer0307, method="pearson")
cor(LogInequalityTest0307, LogCompPer0307,method="pearson")


####################################################################
#Yearly Data Test to Check if there are difference between decades #
####################################################################
#CompPer ~ ALI Cases + Inequality + Production, '08-'12

# '08-'12
ALICases0812 <- as.numeric(ALI_Caseload_Cases[16:20,1]) #Changed Name
InequalityTest0812 <- as.numeric(Inequality[16:20, "Gini coefficient"])
CompPer0812 <- as.numeric(CompPer[24:28,"Philippines"]) 
Prod0812 <- as.numeric(Prod[37:41,"Total"])
print(InequalityTest0812)


#Bind all Data Sets for '08-'12
mydata0812 <- rbind(ALICases0812,InequalityTest0812, CompPer0812, Prod0812)
mydata0812.df <- data.frame(mydata0812)
print(mydata0812.df)

#Simple Regression '08-'012
LReg <- lm(formula= CompPer0812~Prod0812+ALICases0812+InequalityTest0812, data=mydata0812.df)
LReg
summary(LReg)


# LogCompPer ~ LogALI Cases + LogInequality + LogProduction, '08-'12
###################################################################################

#Logging Datasets
LogALICases0812 <- log(as.numeric(ALI_Caseload_Cases[16:20,1]))
LogInequalityTest0812 <- log(as.numeric(Inequality[16:20, "Gini coefficient"]))
LogCompPer0812 <- log(as.numeric(CompPer[24:28, 1])) 
LogProd0812 <- log(as.numeric(Prod[37:41,"Total"]))
print(LogCompPer0812)


#Bind all Data Sets for Log
mydata0812_log <- rbind(LogALICases0812, LogALICases0812, LogCompPer0812, LogProd0812)
mydata0812_log.df <- data.frame(mydata0812_log)
print(mydata0812_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPer0812~LogProd0812+LogALICases0812+LogInequalityTest0812, data=mydata0812_log.df)
LogReg
summary(LogReg)

# Multicollinearity Test
car::vif(LogReg)

#Heteroscedasticity Test
lmtest::bptest(LogReg)
car::ncvTest(LogReg)

#Autocorrelation Test
checkresiduals(LogReg, lag=1)

# Checking for correlations
cor(LogProd0812,LogCompPer0812, method="pearson")
cor(LogALICases0812,LogCompPer0812, method="pearson")
cor(LogInequalityTest0812, LogCompPer0812,method="pearson")

####################################################################
#Yearly Data Test to Check if there are difference between decades #
####################################################################
#CompPer ~ ALI Cases + Inequality + Production, '13-'17

# '13-'17
ALICases1317 <- as.numeric(ALI_Caseload_Cases[21:25,1]) #Changed Name
InequalityTest1317 <- as.numeric(Inequality[21:25, "Gini coefficient"])
CompPer1317 <- as.numeric(CompPer[29:33,"Philippines"]) 
Prod1317 <- as.numeric(Prod[42:46,"Total"])
print(InequalityTest1317)


#Bind all Data Sets for '13-'17
mydata1317 <- rbind(ALICases1317,InequalityTest1317, CompPer1317, Prod1317)
mydata1317.df <- data.frame(mydata1317)
print(mydata1317.df)

#Simple Regression '08-'012
LReg <- lm(formula= CompPer1317~Prod1317+ALICases1317+InequalityTest1317, data=mydata1317.df)
LReg
summary(LReg)


# LogCompPer ~ LogALI Cases + LogInequality + LogProduction, '13-'17
###################################################################################

#Logging Datasets
LogALICases1317 <- log(as.numeric(ALI_Caseload_Cases[21:25,1]))
LogInequalityTest1317 <- log(as.numeric(Inequality[21:25, "Gini coefficient"]))
LogCompPer1317 <- log(as.numeric(CompPer[29:33, 1])) 
LogProd1317 <- log(as.numeric(Prod[42:46,"Total"]))
print(LogCompPer1317)


#Bind all Data Sets for Log
mydata1317_log <- rbind(LogALICases1317, LogALICases1317, LogCompPer1317, LogProd1317)
mydata1317_log.df <- data.frame(mydata1317_log)
print(mydata1317_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPer1317~LogProd1317+LogALICases1317+LogInequalityTest1317, data=mydata1317_log.df)
LogReg
summary(LogReg)

# Multicollinearity Test
car::vif(LogReg)

#Heteroscedasticity Test
lmtest::bptest(LogReg)
car::ncvTest(LogReg)

#Autocorrelation Test
checkresiduals(LogReg, lag=1)

# Checking for correlations
cor(LogProd0812,LogCompPer0812, method="pearson")
cor(LogALICases0812,LogCompPer0812, method="pearson")
cor(LogInequalityTest0812, LogCompPer0812,method="pearson")

