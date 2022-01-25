#######################
library(readxl)
library(xtable)
library(mctest)
library(goft)
library(fitdistrplus)
library(logspline)
library(flexsurv)
library(reldist)
library(dplyr)

#######################
# Loading data:
data = read_xlsx('C:/Users/bramg/Documents/Education_7.xlsx')
data.fillna = data
data.fillna[is.na(data.fillna)] = 0
highschoolCourses = data.fillna[,2:9]
uniData = data.fillna[,10:13]
uniDataBSA = uniData[uniData$`Number of ECT's`>=42,]
uniDataBSA

#######################
pairs(mathUni ~. , data = highschoolCourses)
summary(data)

mathUni = data.fillna$`Mathematics Univ`
dutchUni = data.fillna$`Dutch Univ`

grid <- matrix(c(1,2), nrow =1, ncol = 2)
layout(grid)
plot(denscity(data$`Mathematics Univ`), lwd = 2, main = "Density: Math Uni")
plot(density(data$`Dutch Univ`), lwd = 2, main = "Density: Dutch Uni")

#######################
descdist(mathUni, discrete = FALSE)
descdist(dutchUni, discrete = FALSE)
 
#######################
######## Question 1
# Math Uni
mathMultipleregression.lm = lm(mathUni~., data = highschoolCourses)
plot(density(mathMultipleregression.lm$residuals))
summary(mathMultipleregression.lm)
mathLRSelection = highschoolCourses[,1:]
mathMultipleregression2.lm = lm(mathUni~., data = mathLRSelection)
summary(mathMultipleregression2.lm)
mean(mathMultipleregression.lm$residuals)

qqnorm(mathMultipleregression.lm$residuals)
qqline(mathMultipleregression.lm$residuals, col = "steelblue", lwd = 2)

descdist(mathMultipleregression.lm$residuals, discrete = FALSE)


# Dutch Uni
dutchMultipleregression.lm = lm(dutchUni~., data = highschoolCourses)
summary(dutchMultipleregression.lm)
plot(density(dutchMultipleregression.lm$residuals))
mean(dutchMultipleregression.lm$residuals)
dutchLRSelection = highschoolCourses[,c(1,2,3,5,6,8)]
dutchMultipleregression2.lm = lm(dutchUni~., data = dutchLRSelection)
summary(dutchMultipleregression2.lm)

qqnorm(dutchMultipleregression.lm$residuals)
qqline(dutchMultipleregression.lm$residuals, col = "steelblue", lwd = 2)

descdist(dutchMultipleregression.lm$residuals, discrete = FALSE)


######## Question 2
# Math Uni
math2Multipleregression.lm = lm(`Mathematics Univ`~ `Number of ECT's`, data = uniDataBSA)
summary(math2Multipleregression.lm)

# Dutch Uni
dutch2Multipleregression.lm = lm(`Dutch Univ`~ `Number of ECT's`, data = uniDataBSA)
summary(dutch2Multipleregression.lm)

#######################
cor_data = data.frame(highschoolCourses, data.fillna[,11:13])
cor_data.cor = round(cor(cor_data),2)
write.csv(cor_data.cor, 'C:/Users/bramg/Documents/correlation.csv')

pairs(cor_data)


alpha = data.fillna[data.fillna$Studies=='Dutch',]
alphaCourses = alpha[,2:9]
alphaMath = alpha$`Mathematics Univ`
alphaDutch = alpha$`Dutch Univ`

gamma = data.fillna[data.fillna$Studies=='Law' | data.fillna$Studies=='Antropology' | data.fillna$Studies=='Business Administ.' | data.fillna$Studies=='Economics' | data.fillna$Studies=='Sociology',]
gammaCourses = gamma[,2:9]
gammaMath = gamma$`Mathematics Univ`
gammaDutch = gamma$`Dutch Univ`

beta = data.fillna[data.fillna$Studies=='Chemistry' | data.fillna$Studies=='Computer Science'  | data.fillna$Studies=='Dentistry' | data.fillna$Studies=='Ecomometrics' | data.fillna$Studies=='Farmacology' | data.fillna$Studies=='Geology' | data.fillna$Studies=='Mathematics' | data.fillna$Studies=='Medicine' | data.fillna$Studies=='Physics',]
betaCourses = beta[,2:9]
betaMath = beta$`Mathematics Univ`
betaDutch = beta$`Dutch Univ`


ols = function(y1, data, Title){
  multipleLinearRegression.lm = lm(y1 ~., data=data)
  summary(multipleLinearRegression.lm)
  
  # multipleLinearRegression.lm = lm(y2 ~., data=data)
  # summary(multipleLinearRegression.lm)
} 

ols(alphaDutch, alphaCourses, "Title")

ols(gammaMath, gammaCourses, "Title")

ols(betaDutch, betaCourses, "Title")




















