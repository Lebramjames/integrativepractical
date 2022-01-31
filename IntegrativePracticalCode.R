######## Packages ######## 

library(psych)
library(readxl)
library(xtable)
library(mctest)
library(goft)
library(fitdistrplus)
library(logspline)
library(flexsurv)
library(reldist)
library(dplyr)
library(tseries)
library(regclass)
library(dplyr)
library('ggpubr')
library(SmartEDA)
library(stargazer)
library("tibble")
library(ggplot2)

######### Data ######### 
# Loading data
colorder = c("Dutch", "English", "Mathematics A", "Mathematics C", "Mathematics B",
             "Mathematics D", "Latin","Greek") 
data = read_xlsx('C:/Users/bramg/Desktop/EOR_integrativepractical/Education_7.xlsx')

# Saving dataframes/ renaming columns
colnames(data)[11] = 'ECTs'
colnames(data)[12] = 'MathUni'
colnames(data)[13] = 'DutchUni'
highschoolCourses= data[,2:9]
highschoolCourses.math = data[,2:5]
highschoolCourses.alpha = data[6:9]
mathUni = data$MathUni
dutchUni = data$DutchUni
uniData = data[,12:13]
ECTs = data$ECTs
uniDataBSA= uniData[uniData$`Number of ECT's`>=42,]
combined = data.frame(uniData,highschoolCourses,ECTs)

######### Summary Statistics ######### 

#summary
stargazer(as.data.frame(combined),title="Summary Statistics: Education Data",
          digits=1,flip = FALSE, type = 'latex', summary=TRUE)

#scatterplots
pairs.panels(mathUni ~. , data = highschoolCourses.math,panel=panel.smooth, lwd = 3) 
pairs(dutchUni ~. , data = highschoolCourses.alpha,panel=panel.smooth, lwd = 3)

#correlations
cor.cor = cor(data.frame(mathUni, dutchUni, highschoolCourses),use = "pairwise.complete.obs")
stargazer(cor.cor, align=TRUE ,no.space=TRUE,type="latex", title="Correlation Matrix: Education Data")

# Density
par(mfrow = c(1,2))
plot(density(data$MathUni), lwd = 2, main = "Density: Math Uni")
plot(density(data$DutchUni), lwd = 2, main = "Density: Dutch Uni")

par(mfrow = c(1,2))
descdist(mathUni, discrete = FALSE)
descdist(dutchUni, discrete = FALSE)

######### Question 1 ######### 

# Group 1: Gymnasium Data
gymnasiumA = na.omit(data[data$`Mathematics A`!=0, c('Dutch', 'English', 'Mathematics A', 'Latin', 'Greek')])
gymnasiumA.math = na.omit(data[data$`Mathematics A` !=0 & !is.na(data$Latin), c('MathUni')])
gymnasiumA.dutch = na.omit(data[data$`Mathematics A` !=0 & !is.na(data$Latin), c('DutchUni')])

gymnasiumB = na.omit(data[is.na(data$`Mathematics D`) & data$`Mathematics B` !=0, c('Dutch', 'English', 'Mathematics B', 'Latin', 'Greek')])
gymnasiumB.math = na.omit(data[is.na(data$`Mathematics D`) & data$`Mathematics B`& !is.na(data$Latin), c('MathUni')])
gymnasiumB.dutch = na.omit(data[is.na(data$`Mathematics D`) & data$`Mathematics B`& !is.na(data$Latin), c('DutchUni')])

gymnasiumC = na.omit(data[data$`Mathematics C` !=0, c('Dutch', 'English', 'Mathematics C', 'Latin', 'Greek')])
gymnasiumC.math = na.omit(data[data$`Mathematics C` !=0 & !is.na(data$Latin), c('MathUni')])
gymnasiumC.dutch = na.omit(data[data$`Mathematics C` !=0 & !is.na(data$Latin), c('DutchUni')])

gymnasiumBD = na.omit(data[data$`Mathematics B` !=0 & data$`Mathematics D` != 0 , c('Dutch', 'English', 'Mathematics B','Mathematics D', 'Latin', 'Greek')])
gymnasiumBD.math = na.omit(data[data$`Mathematics B` !=0 & data$`Mathematics D` != 0 & !is.na(data$Latin), c('MathUni')])
gymnasiumBD.dutch = na.omit(data[data$`Mathematics B` !=0 & data$`Mathematics D` != 0 & !is.na(data$Latin), c('DutchUni')])

# Group 1: Atheneum Data
athenaeumA = na.omit(data[is.na(data$Greek)& data$`Mathematics A` !=0, c('Dutch', 'English', 'Mathematics A')])
athenaeumA.math = na.omit(data[data$`Mathematics A` !=0 & is.na(data$Latin), c('MathUni')])
athenaeumA.dutch = na.omit(data[data$`Mathematics A` !=0 & is.na(data$Latin), c('DutchUni')])

athenaeumB = na.omit(data[is.na(data$Greek)& is.na(data$`Mathematics D`) & data$`Mathematics B` !=0, c('Dutch', 'English', 'Mathematics B')])
athenaeumB.math = na.omit(data[is.na(data$`Mathematics D`) & data$`Mathematics B`& is.na(data$Latin), c('MathUni')])
athenaeumB.dutch = na.omit(data[is.na(data$`Mathematics D`) & data$`Mathematics B`& is.na(data$Latin), c('DutchUni')])

athenaeumC = na.omit(data[is.na(data$Greek) & data$`Mathematics C` !=0, c('Dutch', 'English', 'Mathematics C')])
athenaeumC.math = na.omit(data[data$`Mathematics C` !=0 & is.na(data$Latin), c('MathUni')])
athenaeumC.dutch = na.omit(data[data$`Mathematics C` !=0 & is.na(data$Latin), c('DutchUni')])

athenaeumBD = na.omit(data[is.na(data$Greek)& data$`Mathematics B` !=0 & data$`Mathematics D` != 0, c('Dutch', 'English', 'Mathematics B', 'Mathematics D')])
athenaeumBD.math = na.omit(data[data$`Mathematics B` !=0 & data$`Mathematics D` != 0 & is.na(data$Latin), c('MathUni')])
athenaeumBD.dutch = na.omit(data[data$`Mathematics B` !=0 & data$`Mathematics D` != 0 & is.na(data$Latin), c('DutchUni')])

###### Linear Regression Models
# Math
gymnasiumAMath.lm = lm(formula = gymnasiumA.math$MathUni ~. , gymnasiumA)
gymnasiumBMath.lm = lm(formula = gymnasiumB.math$MathUni ~. , gymnasiumB)
gymnasiumCMath.lm = lm(formula = gymnasiumC.math$MathUni ~. , gymnasiumC)
gymnasiumDMath.lm = lm(formula = gymnasiumBD.math$MathUni ~. , gymnasiumBD)

athenaeumAMath.lm = lm(formula = athenaeumA.math$MathUni ~. , athenaeumA)
athenaeumBMath.lm = lm(formula = athenaeumB.math$MathUni ~. , athenaeumB)
athenaeumCMath.lm = lm(formula = athenaeumC.math$MathUni ~. , athenaeumC)
athenaeumBDMath.lm = lm(formula = athenaeumBD.math$MathUni ~. , athenaeumBD)

# Visualizing Math
stargazer(gymnasiumA.lm, gymnasiumBMath.lm, gymnasiumCMath.lm, gymnasiumDMath.lm,
          align=TRUE ,no.space=TRUE,type="text",omit.stat=c("LL","ser","f"))

stargazer(athenaeumAMath.lm, athenaeumBMath.lm, athenaeumCMath.lm, athenaeumBDMath.lm,
          align=TRUE ,no.space=TRUE,type="text",omit.stat=c("LL","ser","f"))

# Dutch
gymnasiumADutch.lm = lm(formula = gymnasiumA.dutch$DutchUni ~. , gymnasiumA)
gymnasiumBDutch.lm = lm(formula = gymnasiumB.dutch$DutchUni ~. , gymnasiumB)
gymnasiumCDutch.lm = lm(formula = gymnasiumC.dutch$DutchUni ~. , gymnasiumC)
gymnasiumBDDutch.lm = lm(formula = gymnasiumBD.dutch$DutchUni ~. , gymnasiumBD)

athenaeumADutch.lm = lm(formula = athenaeumA.dutch$DutchUni ~. , athenaeumA)
athenaeumBDutch.lm = lm(formula = athenaeumB.dutch$DutchUni ~. , athenaeumB)
athenaeumCDutch.lm = lm(formula = athenaeumC.dutch$DutchUni ~. , athenaeumC)
athenaeumBDDutch.lm = lm(formula = athenaeumBD.dutch$DutchUni ~. , athenaeumBD)

# Visualizing Dutch
stargazer(gymnasiumADutch.lm, gymnasiumBDutch.lm, gymnasiumCDutch.lm, gymnasiumBDDutch.lm,
          no.space=TRUE,omit.stat=c("LL","ser","f"))
stargazer(athenaeumADutch.lm, athenaeumBDutch.lm, athenaeumCDutch.lm, athenaeumBDDutch.lm,
          align=TRUE ,no.space=TRUE,type="text",omit.stat=c("LL","ser","f"))


# Interesting Models
stargazer(gymnasiumBMath.lm, gymnasiumAMath.lm, gymnasiumBDutch.lm, athenaeumBMath.lm,athenaeumBDMath.lm,
          column.labels=c("Gymn.","Gymn.", "Gymn.", "Ath.", "Ath.") ,
          title="OLS Regression Results Gymnasium and Atheneum", no.space=TRUE, omit.stat=c("LL","ser",'F'))
stargazer(mean(gymnasiumAMath.lm$residuals) ,mean(gymnasiumBMath.lm$residuals), type = 'text',
          title="OLS Regression Results Gymnasium", no.space=TRUE, omit.stat=c("LL","ser",'F'))

# Appendix
stargazer(gymnasiumAMath.lm, gymnasiumBMath.lm, gymnasiumCMath.lm, gymnasiumDMath.lm,
          gymnasiumADutch.lm, gymnasiumBDutch.lm, gymnasiumCDutch.lm, gymnasiumBDDutch.lm,
          title="OLS Regression Results Gymnasium", no.space=TRUE, omit.stat=c("LL","ser",'F'))
stargazer(athenaeumAMath.lm, athenaeumBMath.lm, athenaeumCMath.lm, athenaeumBDMath.lm,
          athenaeumADutch.lm, athenaeumBDutch.lm, athenaeumCDutch.lm, athenaeumBDDutch.lm,
          title="OLS Regression Results Atheneum", no.space=TRUE, omit.stat=c("LL","ser","f"))

# Studies
categories = unique(data$Studies) 
studiesDutch.lm = lm(formula = data$DutchUni ~ as.factor(data$Studies))
studiesMath.lm = lm(formula = data$MathUni ~ as.factor(data$Studies))
stargazer(studiesDutch.lm, studiesMath.lm, 
          covariate.labels = categories, type='text',
          title= 'OLS Regression Results Studies', align=TRUE ,no.space=TRUE,omit.stat=c("LL","ser"), report = ('c*t'), nobs=TRUE)

# OLS Assumptions
par(mfrow=c(2,3))
qqnorm(gymnasiumBMath.lm$residuals, main= 'Gymnasium B, Math')
qqline(gymnasiumBMath.lm$residuals, col = "2", lwd = 2)
qqnorm(gymnasiumAMath.lm$residuals, main= 'Gymnasium A, Math')
qqline(gymnasiumAMath.lm$residuals, col = "2", lwd = 2)
qqnorm(gymnasiumBDutch.lm$residuals, main= 'Gymnasium B, Dutch')
qqline(gymnasiumBDutch.lm$residuals, col = "2", lwd = 2)
qqnorm(athenaeumBMath.lm$residuals, main= 'Athenaeum A, Math')
qqline(athenaeumBMath.lm$residuals, col = "2", lwd = 2)
qqnorm(athenaeumBDMath.lm$residuals, main= 'Athenaeum BD, Math')
qqline(athenaeumBDMath.lm$residuals, col = "2", lwd = 2)
qqnorm(athenaeumCMath.lm$residuals, main= 'Athenaeum C, Math')
qqline(athenaeumCMath.lm$residuals, col = "2", lwd = 2)


######### Question 2 ######### 

ectSuccess = data[data$ECTs>=42 , c('MathUni', 'DutchUni' ,  "ECTs")]
ectFail = data[data$ECTs<42 , c('MathUni', 'DutchUni',  "ECTs")]

testRegression = lm(formula = MathUni~ ECTs, data= data)
summary(testRegression)
testRegression2 = lm(formula = DutchUni~ ECTs, data= data)
summary(testRegression2)

dataCor = data[,11:13]
cor(dataCor)
pairs(dataCor, panel=panel.smooth, lwd = 3)

dataECT = data.frame(ectSuccess, ectFail)

stargazer(testRegression, testRegression2, type = 'latex', omit.stat=c("LL","ser"), dep.var.labels=c("Math Uni", 'Dutch Uni'))

par(mfrow=c(1,2))
qqnorm(testRegression$residuals, main= 'Mathematics University')
qqline(testRegression$residuals, col = "2", lwd = 2)
qqnorm(testRegression2$residuals, main= 'Dutch University')
qqline(testRegression2$residuals, col = "2", lwd = 2)

# ECT's
ectMathSuccess.lm = lm(formula = MathUni ~ ECTs,data =  ectSuccess)
ectMathFail.lm = lm(formula = MathUni ~ ECTs, data = ectFail)

ectDutchSuccess.lm = lm(formula = DutchUni ~ ECTs,data =  ectSuccess)
ectDutchFail.lm = lm(formula = DutchUni ~ ECTs, data = ectFail)

stargazer(ectDutchSuccess.lm,ectDutchFail.lm, ectMathSuccess.lm, ectMathFail.lm,
          dep.var.labels=c("Dutch Success(1) / Fail(2)","Math Success(3) / Fail (4)"), title= 'OLS Regression Results ECTS', type = 'text', align=TRUE ,no.space=TRUE,omit.stat=c("LL", 'F'))

par(mfrow=c(2,2))
qqnorm(ectDutchSuccess.lm$residuals, main= 'Dutch Succes')
qqline(ectDutchSuccess.lm$residuals, col = "2", lwd = 2)
qqnorm(ectDutchFail.lm$residuals, main= 'Dutch Fail')
qqline(ectDutchFail.lm$residuals, col = "2", lwd = 2)
qqnorm(ectMathSuccess.lm$residuals, main= 'Math Succes')
qqline(ectMathSuccess.lm$residuals, col = "2", lwd = 2)
qqnorm(ectMathFail.lm$residuals, main= 'Math Fail')
qqline(ectMathFail.lm$residuals, col = "2", lwd = 2)

par(mfrow = c(1,2))
plot( ectSuccess$`Number of ECT's`, ectSuccess$MathUni)
plot(ectSuccess$`Number of ECT's`, ectSuccess$DutchUni, )
plot(ectSuccess$MathUni, ectMathSuccess.lm$fitted.values)
