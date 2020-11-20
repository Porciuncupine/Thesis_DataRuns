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

#Loading and Transposing Datasets
Inequality <- t(Thesis.Dataset...Gini.Coefficient)
ALI_Cases <-t(Thesis.Dataset...RegionCases)
CompRate <- t(Thesis.Dataset...RegCompRate)
Prod <- t(Thesis.Dataset...RegionProd)
print(ALI_Cases)

#RegCompPer ~ Reg ALI Cases + Inequality + Reg Production
#Region 1
ALICasesReg1 <- as.numeric(ALI_Cases[,"I"])
InequalityTest <- as.numeric(Inequality[6:26, "Gini coefficient"])
CompPerReg1 <- as.numeric(CompRate[,"I"])
ProdReg1 <- as.numeric(Prod[,"R1"])
print(ALICasesReg1)

#Bind all Data Sets for Region 1
mydataReg1 <- rbind(ALICasesReg1,InequalityTest, CompPerReg1, ProdReg1)
mydataReg1.df <- data.frame(mydataReg1)
print(mydataReg1.df)

#Simple Regression Test Region 1
LReg <- lm(formula= CompPerReg1~ProdReg1+ALICasesReg1+InequalityTest, data=mydataReg1.df)
LReg
summary(LReg)

#logging datasets
LogALICasesReg1 <- log(as.numeric(ALI_Cases[,"I"]))
LogInequalityTest <- log(as.numeric(Inequality[6:26, "Gini coefficient"]))
LogCompPerReg1 <- log(as.numeric(CompRate[,"I"]))
LogProdReg1 <- log(as.numeric(Prod[,"R1"]))
print(LogProdReg1)

#Bind all Data Sets for Region
mydataReg1_log <- rbind(LogALICasesReg1, LogInequalityTest, LogCompPerReg1, LogProdReg1)
mydataReg1_log.df <- data.frame(mydataReg1_log)
print(mydataReg1_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPerReg1~LogProdReg1+LogALICasesReg1+LogInequalityTest, data=mydataReg1_log.df)
LogReg
summary(LogReg)


##################################################################################
#Region 3
ALICasesReg3 <- as.numeric(ALI_Cases[,"III"])
InequalityTest <- as.numeric(Inequality[6:26, "Gini coefficient"])
CompPerReg3 <- as.numeric(CompRate[,"III"])
ProdReg3 <- as.numeric(Prod[,"R3"])
print(ProdReg3)

#Bind all Data Sets for Region 3
mydataReg3 <- rbind(ALICasesReg3,InequalityTest, CompPerReg3, ProdReg3)
mydataReg3.df <- data.frame(mydataReg3)
print(mydataReg3.df)

#Simple Regression Test Region 3
LReg <- lm(formula= CompPerReg3~ProdReg3+ALICasesReg3+InequalityTest, data=mydataReg3.df)
LReg
summary(LReg)

#logging datasets
LogALICasesReg3 <- log(as.numeric(ALI_Cases[,"III"]))
LogInequalityTest <- log(as.numeric(Inequality[6:26, "Gini coefficient"]))
LogCompPerReg3 <- log(as.numeric(CompRate[,"III"]))
LogProdReg3 <- log(as.numeric(Prod[,"R3"]))
print(LogProdReg3)

#Bind all Data Sets for Region 3 
mydataReg3_log <- rbind(LogALICasesReg3, LogInequalityTest, LogCompPerReg3, LogProdReg3)
mydataReg3_log.df <- data.frame(mydataReg3_log)
print(mydataReg3_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPerReg3~LogProdReg3+LogALICasesReg3+LogInequalityTest, data=mydataReg3_log.df)
LogReg
summary(LogReg)

##################################################################################
#Region 4A
ALICasesReg4 <- as.numeric(ALI_Cases[,"IV-A"])
InequalityTest <- as.numeric(Inequality[6:26, "Gini coefficient"])
CompPerReg4 <- as.numeric(CompRate[,"IV-A"])
ProdReg4 <- as.numeric(Prod[,"R4A"])
print(ProdReg4)

#Bind all Data Sets for Region 4
mydataReg4 <- rbind(ALICasesReg4,InequalityTest, CompPerReg4, ProdReg4)
mydataReg4.df <- data.frame(mydataReg4)
print(mydataReg4.df)

#Simple Regression Test Region 4
LReg <- lm(formula= CompPerReg4~ProdReg4+ALICasesReg4+InequalityTest, data=mydataReg4.df)
LReg
summary(LReg)

#logging datasets
LogALICasesReg4 <- log(as.numeric(ALI_Cases[,"IV-A"]))
LogInequalityTest <- log(as.numeric(Inequality[6:26, "Gini coefficient"]))
LogCompPerReg4 <- log(as.numeric(CompRate[,"IV-A"]))
LogProdReg4 <- log(as.numeric(Prod[,"R4A"]))
print(LogProdReg4)

#Bind all Data Sets for Region 4
mydataReg4_log <- rbind(LogALICasesReg4, LogInequalityTest, LogCompPerReg4, LogProdReg4)
mydataReg4_log.df <- data.frame(mydataReg4_log)
print(mydataReg4_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPerReg4~LogProdReg4+LogALICasesReg4+LogInequalityTest, data=mydataReg4_log.df)
LogReg
summary(LogReg)

##################################################################################
#Region 6
ALICasesReg6 <- as.numeric(ALI_Cases[,"VI"])
InequalityTest <- as.numeric(Inequality[6:26, "Gini coefficient"])
CompPerReg6 <- as.numeric(CompRate[,"VI"])
ProdReg6 <- as.numeric(Prod[,"R6"])
print(ProdReg6)

#Bind all Data Sets for Region 6
mydataReg6 <- rbind(ALICasesReg6,InequalityTest, CompPerReg6, ProdReg6)
mydataReg6.df <- data.frame(mydataReg6)
print(mydataReg6.df)

#Simple Regression Test Region 6
LReg <- lm(formula= CompPerReg6~ProdReg6+ALICasesReg6+InequalityTest, data=mydataReg6.df)
LReg
summary(LReg)

#logging datasets
LogALICasesReg6 <- log(as.numeric(ALI_Cases[,"VI"]))
LogInequalityTest <- log(as.numeric(Inequality[6:26, "Gini coefficient"]))
LogCompPerReg6 <- log(as.numeric(CompRate[,"VI"]))
LogProdReg6 <- log(as.numeric(Prod[,"R6"]))
print(LogProdReg6)

#Bind all Data Sets for Region 6
mydataReg6_log <- rbind(LogALICasesReg6, LogInequalityTest, LogCompPerReg6, LogProdReg6)
mydataReg6_log.df <- data.frame(mydataReg6_log)
print(mydataReg6_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPerReg6~LogProdReg6+LogALICasesReg6+LogInequalityTest, data=mydataReg6_log.df)
LogReg
summary(LogReg)

##################################################################################
#Region 12
ALICasesReg12 <- as.numeric(ALI_Cases[,"XII"])
InequalityTest <- as.numeric(Inequality[6:26, "Gini coefficient"])
CompPerReg12 <- as.numeric(CompRate[,"XII"])
ProdReg12 <- as.numeric(Prod[,"R12"])
print(ProdReg12)

#Bind all Data Sets for Region 12
mydataReg12 <- rbind(ALICasesReg12,InequalityTest, CompPerReg12, ProdReg12)
mydataReg12.df <- data.frame(mydataReg12)
print(mydataReg12.df)

#Simple Regression Test Region 1
LReg <- lm(formula= CompPerReg12~ProdReg12+ALICasesReg12+InequalityTest, data=mydataReg12.df)
LReg
summary(LReg)

#logging datasets
LogALICasesReg12 <- log(as.numeric(ALI_Cases[,"XII"]))
LogInequalityTest <- log(as.numeric(Inequality[6:26, "Gini coefficient"]))
LogCompPerReg12 <- log(as.numeric(CompRate[,"XII"]))
LogProdReg12 <- log(as.numeric(Prod[,"R12"]))
print(LogProdReg12)

#Bind all Data Sets for Region 12
mydataReg12_log <- rbind(LogALICasesReg12, LogInequalityTest, LogCompPerReg12, LogProdReg12)
mydataReg12_log.df <- data.frame(mydataReg12_log)
print(mydataReg12_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPerReg12~LogProdReg12+LogALICasesReg12+LogInequalityTest, data=mydataReg12_log.df)
LogReg
summary(LogReg)


##################################################################################
#Region ARMM
ALICasesRegA <- as.numeric(ALI_Cases[,"BARMM"])
InequalityTest <- as.numeric(Inequality[6:26, "Gini coefficient"])
CompPerRegA <- as.numeric(CompRate[,"BARMM"])
ProdRegA <- as.numeric(Prod[,"ARMM"])
print(ProdRegA)


#Bind all Data Sets for Region ARMM
mydataRegA <- rbind(ALICasesRegA,InequalityTest, CompPerRegA, ProdRegA)
mydataRegA.df <- data.frame(mydataRegA)
print(mydataRegA.df)

#Simple Regression Test Region ARMM
LReg <- lm(formula= CompPerRegA~ProdRegA+ALICasesRegA+InequalityTest, data=mydataRegA.df)
LReg
summary(LReg)

#logging datasets
LogALICasesRegA <- log(as.numeric(ALI_Cases[,"BARMM"]))
LogInequalityTest <- log(as.numeric(Inequality[6:26, "Gini coefficient"]))
LogCompPerRegA <- log(as.numeric(CompRate[,"BARMM"]))
LogProdRegA <- log(as.numeric(Prod[,"ARMM"]))
print(LogProdRegA)

#Bind all Data Sets for Region ARMM
mydataRegA_log <- rbind(LogALICasesRegA, LogInequalityTest, LogCompPerRegA, LogProdRegA)
mydataRegA_log.df <- data.frame(mydataRegA_log)
print(mydataRegA_log.df)

# LogCompPer ~ LogALI Cases + LogInequality + LogProduction
LogReg <- lm(formula = LogCompPerRegA~LogProdRegA+LogALICasesRegA+LogInequalityTest, data=mydataRegA_log.df)
LogReg
summary(LogReg)
