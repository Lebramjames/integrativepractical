######## installing Packages ######## 

# install.packages("readxl")
# install.packages("xtable")
# install.packages("mctest")
# install.packages("goft")
# install.packages("fitdistrplus")
# install.packages("logspline")
# install.packages("flexsurv")
# install.packages("reldist")
# install.packages("dplyr")
# install.packages("tseries")
# install.packages("regclass")
# install.packages("dplyr")
# install.packages("ggpubr")
# install.packages("SmartEDA")

######## Packages ######## 

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

######## Randomness ######## 

set.seed(1234)


######## Loading Data ######## 

data = read_xlsx('C:/Users/bgn630/Downloads/Education_7.xlsx')
colnames(data)[12] = 'MathUni'
colnames(data)[13] = 'DutchUni'

#Saving Variables/ Columns (All)
highschoolCourses= data[,2:9]
highschoolCourses.math = data[,2:5]
highschoolCourses.alpha = data[6:9]

mathUni = data$MathUni
dutchUni = data$DutchUni

uniData = data[,10:13]
uniDataBSA= uniData[uniData$`Number of ECT's`>=42,]

######### Summary ##############

# pairs(mathUni ~. , data = highschoolCourses)
# pairs(dutchUni ~. , data = highschoolCourses)

summary(data)

######### Density Dependent Variables ######### 

grid1 <- matrix(c(1,2), nrow =1, ncol = 2)
layout(grid1)
plot(density(data$MathUni), lwd = 2, main = "Density: Math Uni")
plot(density(data$`Dutch Univ`), lwd = 2, main = "Density: Dutch Uni")

descdist(mathUni, discrete = FALSE)
descdist(dutchUni, discrete = FALSE)
 
######### Question 1: Wiskunde vakken ######### 

pairs(mathUni ~. , data = highschoolCourses.math, panel=panel.smooth)

mathA.lm = lm(mathUni ~ `Mathematics A` , data = highschoolCourses.math)
mathC.lm = lm(mathUni ~ `Mathematics C` , data = highschoolCourses.math)
mathB.lm = lm(mathUni ~ `Mathematics B` , data = highschoolCourses.math)
mathD.lm = lm(mathUni ~ `Mathematics D` , data = highschoolCourses.math)
mathBD.lm = lm(mathUni ~ `Mathematics B` + `Mathematics D`, data = highschoolCourses.math)

grid2 <- matrix(c(1:6), nrow =2, ncol = 3)
layout(grid2)
qqnorm(mathA.lm$residuals, main= 'mathA.lm')
qqline(mathA.lm$residuals, col = "steelblue", lwd = 2)
qqnorm(mathC.lm$residuals, main= 'mathC.lm')
qqline(mathC.lm$residuals, col = "steelblue", lwd = 2)
qqnorm(mathB.lm$residuals, main= 'mathB.lm')
qqline(mathB.lm$residuals, col = "steelblue", lwd = 2)
qqnorm(mathD.lm$residuals, main= 'mathD.lm')
qqline(mathD.lm$residuals, col = "steelblue", lwd = 2)
qqnorm(mathBD.lm$residuals, main= 'mathBD.lm')
qqline(mathBD.lm$residuals, col = "steelblue", lwd = 2)

corMath.data = data.frame(mathUni, highschoolCourses.math)
corMath.cor = cor(corMath.data, use = "pairwise.complete.obs")
VIF(mathBD.lm)

######### Question 1: Alpha vakken ######### 

pairs(dutchUni ~. , data = highschoolCourses.alpha, panel=panel.smooth)

# Dutch Uni
english.lm = lm(dutchUni ~ `English` , data = highschoolCourses.alpha)
dutch.lm = lm(dutchUni ~ `Dutch` , data = highschoolCourses.alpha)
latin.lm = lm(dutchUni ~ `Latin` , data = highschoolCourses.alpha)
greek.lm = lm(dutchUni ~ `Greek` , data = highschoolCourses.alpha)
englishDutch.lm = lm(dutchUni ~ `English` + `Dutch`, data = highschoolCourses.alpha)
latinGreek.lm = lm(dutchUni ~ `Latin` + `Greek`, data = highschoolCourses.alpha)

summary(english.lm)
summary(dutch.lm)
summary(latin.lm)
summary(greek.lm)
summary(englishDutch.lm)

corAlpha.data = data.frame(dutchUni, highschoolCourses.alpha)
corAlpha.cor = cor(cor.data, use = "pairwise.complete.obs")

alpha.lm = lm(dutchUni ~ . , data=highschoolCourses.alpha)
VIF(alpha.lm)

######### Question 1: Studie vakken ######### 

categories = unique(data$Studies) 
data$Studies = as.factor(data$Studies)
levels(data$Studies)

grid3 <- matrix(c(1), nrow =1, ncol = 1)
layout(grid3)
barplot(prop.table(table(data$Studies)))


group_by(data, data$Studies)  %>%
  dplyr::summarise( count = n(),
           mean = mean(data$MathUni, na.rm = TRUE),
           sd = sd(data$MathUni, na.rm = TRUE)
           )

ggboxplot(data, x="Studies", y="MathUni", 
          color = "Studies", palette = c(1:15),
          order = c(levels(data$Studies)),
          ylab = "math Uni Gtae", xlab = "Studies")

group_by(data, data$Studies)  %>%
  dplyr::summarise( count = n(),
                    mean = mean(data$DutchUni, na.rm = TRUE),
                    sd = sd(data$DutchUni, na.rm = TRUE)
  )

ggboxplot(data, x="Studies", y="DutchUni", 
          color = "Studies", palette = c(1:15),
          order = c(levels(data$Studies)),
          ylab = "Dutch Uni Grade", xlab = "Studies")


res.aov = aov(MathUni ~ Studies, data = data )
summary(res.aov)

pairwiset = pairwise.t.test(data$MathUni, data$Studies,
                p.adjust.method = "BH")


