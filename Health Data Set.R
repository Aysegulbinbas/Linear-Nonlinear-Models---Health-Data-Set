##### Dictionary #####
#SEQN - respondent sequence number
#DMDHHSIZ - Total number of people in the Household
#INDHHIN2 - Total household income (reported as a range value in dollars)
#RIAGENDR - Gender of the participant
#RIDAGEYR - Age in years of the participant at the time of screening. Individuals 80 and over are topcoded at 80 years of age
# !!! RESPONSE!!! BPXSY1 - Systolic: Blood pressure (first reading) mm Hg (Blood Pressure)
#BMXBMI - Body Mass Index (kg/m**2) (Blood Pressure)
#BPXPULS - Pulse regular or irregular (Blood Pressure)
#BPXML1 - MIL: maximum inflation levels (mm Hg) (Blood Pressure)
#DR1TALCO - Alcohol (gm) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TSODI - Sodium (mg) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TPOTA - Potassium (mg) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TCALC - Calcium (mg) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TMAGN - Magnesium (mg) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TPROT - Protein (gm) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TKCAL - Energy (kcal) (Dietary Interview - Total Nutrient Intakes, First Day)
#SMQ020 - These next questions are about cigarette smoking and other tobacco use. {Have you/Has SP} smoked at least 100 cigarettes in {your/his/her} entire life? (Questionnaire Variable List)


##### Data Manipulation #####
##### Dictionary #####
#SEQN - respondent sequence number
#DMDHHSIZ - Total number of people in the Household
#INDHHIN2 - Total household income (reported as a range value in dollars)
#RIAGENDR - Gender of the participant
#RIDAGEYR - Age in years of the participant at the time of screening. Individuals 80 and over are topcoded at 80 years of age
# !!! RESPONSE!!! BPXSY1 - Systolic: Blood pressure (first reading) mm Hg (Blood Pressure)
#BMXBMI - Body Mass Index (kg/m**2) (Blood Pressure)
#BPXPULS - Pulse regular or irregular (Blood Pressure)
#BPXML1 - MIL: maximum inflation levels (mm Hg) (Blood Pressure)
#DR1TALCO - Alcohol (gm) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TSODI - Sodium (mg) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TPOTA - Potassium (mg) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TCALC - Calcium (mg) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TMAGN - Magnesium (mg) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TPROT - Protein (gm) (Dietary Interview - Total Nutrient Intakes, First Day)
#DR1TKCAL - Energy (kcal) (Dietary Interview - Total Nutrient Intakes, First Day)
#SMQ020 - These next questions are about cigarette smoking and other tobacco use. {Have you/Has SP} smoked at least 100 cigarettes in {your/his/her} entire life? (Questionnaire Variable List)


##### Data Manipulation #####


suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(PerformanceAnalytics))
suppressPackageStartupMessages(library(olsrr))
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(bestNormalize))
suppressPackageStartupMessages(library(LambertW))
suppressPackageStartupMessages(library(robustbase))
suppressPackageStartupMessages(library(regclass))
suppressPackageStartupMessages(library(gtsummary))
suppressPackageStartupMessages(library(mdscore))
suppressPackageStartupMessages(library(dplyr))

health <- read.csv("C:/Users/ABRA/Desktop/nhanes_systolic.csv", header = T, sep = ";")
health
str(health)
summary(health)

colnames(health) <- c("resp_seq_num", "total_num_people_household", "total_household_income", "sex", "age", "blood_pressure", "bmi", "pulse", "max_inflation",
                      "alcohol", "sodium", "potassium", "calcium", "magnesium", "protein", "energy", "smoking"  )
str(health)
summary(health)
sum(is.na(health))
sum(is.null(health))

health$sex <- as.factor(health$sex)
health$pulse <- as.factor(health$pulse)
health$smoking <- as.factor(health$smoking)

health$bmi <- gsub(",", ".", health$bmi)
health$alcohol <- gsub(",", ".", health$alcohol)
health$protein <- gsub(",", ".", health$protein)

health$alcohol <- as.numeric(health$alcohol)
health$bmi <- as.numeric(health$bmi)
health$protein <- as.numeric(health$protein)

str(health)
summary(health)
sum(is.na(health))
sum(is.null(health))
range(health$total_num_people_household)
range(health$total_household_income)

##### Research Questions & Proposed Methods #####

###### RQ : What are the significant variables correlated with the blood pressure? #######
# response : blood pressure
# proposed method : scatter plots of the blood pressure vs all other variables, variable selection method, multiple regression

# H0 : All coefficents are equal to zero
# H1 : At least one of them is not equal to zero

###Variable selection method: Stepwise regression in both direction based on p-value and aic values used for create the best model.

###Stepwise regression in both direction based on p-value
model<- lm(blood_pressure ~ ., data=health)
k1<-ols_step_both_p(model)
plot(k1)
ols_step_both_p(model,details = TRUE)

model1<- lm(blood_pressure ~ max_inflation+bmi+total_num_people_household+sex, data=health)
summary(model1)
mse1<-mean(model1$residuals^2)

###Stepwise regression in both direction based on AIC
k2<-ols_step_both_aic(model)
plot(k2)
ols_step_both_aic(model,details = TRUE)

model2<-lm(blood_pressure ~ max_inflation+bmi+total_num_people_household+sex+pulse+resp_seq_num+alcohol+age, data=health)
summary(model2)
mse2<-mean(model2$residuals^2)
mse1
mse2

###Final Decision: Since the second model and the first models have almost same adjusted r-sqrt values and mse values,
###The first model includes less predictors, it is better to continue with the first model which
###is a stepwise regression model based on p value. The model includes 4 predictors which are max inflation, bmi, total number of people in the household and sex.


###Model with the significant interaction terms.
model1<- lm(blood_pressure ~ max_inflation+bmi+total_num_people_household+sex+max_inflation*total_num_people_household+max_inflation*sex+total_num_people_household*sex, data=health)
summary(model1)
plot(model1)
shapiro.test(health$blood_pressure)

###Response is not normal, transformation is needed. 
###Let's try boxcox transformation to normalize the response.
boxcox(model1)
###Since confidence interval for lambda not includes 1, and likelihood is around 0.5, sqrt transformation is may be a good idea to normalize the response.
model1<- lm(sqrt(blood_pressure) ~ max_inflation+bmi+total_num_people_household+sex+max_inflation*total_num_people_household+max_inflation*sex+total_num_people_household*sex, data=health)
summary(model1)
plot(model1)
shapiro.test(sqrt(health$blood_pressure))
###Residuals performed better with the transformation, however there is still non-normality problem with the response variable.

shapiro.test(log(health$blood_pressure))
shapiro.test(1/(health$blood_pressure))
###After the these transformations, response is still not normally distributed. 


###Normalizing response with the "bestnormalize" package
BN_obj <- bestNormalize(health$blood_pressure, allow_boxcox = TRUE)
BN_obj

gx <- predict(BN_obj)

x2 <- predict(BN_obj, newdata = gx, inverse = TRUE)

all.equal(x2, health$blood_pressure)

shapiro.test(gx)
###Still not normal.

###Distribution of response is right skewed, and since it is a member of exponential family, gamma, using glm is an alternative. 
model1 <- glm(sqrt(blood_pressure) ~ max_inflation+bmi+total_num_people_household+sex+max_inflation*total_num_people_household+max_inflation*sex+total_num_people_household*sex, data=health, family = Gamma("identity"))
summary(model1)
###Total number of people in the household is not significant anymore, can be dropped from the model.
model1 <- glm(sqrt(blood_pressure) ~ (max_inflation)+(bmi)+sex+max_inflation*sex, data=health, family = Gamma("identity"))
summary(model1)
plot(model1)

###Check the relationships between response and predictors are linear.
plot(health$max_inflation,health$blood_pressure) #Linear
plot(health$bmi,health$blood_pressure) #seems linear but non-constant variance problem.

plot(model1$fitted.values,model1$residuals)
abline(h=0)
###Now, there is no obvious pattern in the plot, variance seems constant.

ggplot(health,aes(max_inflation+bmi,sqrt(blood_pressure),col=sex))+ 
  geom_point() + geom_smooth(method="glm",method.args= list(family="Gamma"))

summary(model1)

plot(fitted(model),model1$residuals)
abline(h=0)

qqnorm(model1$residuals)
qqline(model1$residuals)

###Checking multicollenarity 
VIF(model1)
###Since VIF values are smaller than 4 for max inflation and bmi, we can say these variables are not highly correlated with eachother, it is not a big problem. However for the variable sex, vif value is much more higher than 5, it means sex is highly correlated with other variables.
###What if we remove inteaction of sex and max inflation*sex from the model?
model1 <- glm(sqrt(blood_pressure) ~ (max_inflation)+(bmi)+sex, data=health, family = Gamma("identity"))
summary(model1)
VIF(model1)
###No multicollenarity problem now and sex is not significant anymore, can be removed from the model.
model2 <- glm(sqrt(blood_pressure) ~ (max_inflation)+(bmi), data=health, family = Gamma("identity"))
summary(model1)
VIF(model2)

model1 <- glm(sqrt(blood_pressure) ~ (max_inflation)+(bmi), data=health, family = Gamma("identity"))
summary(model1)

model2 <- glm(sqrt(blood_pressure) ~ (max_inflation)+(bmi), data=health, family = Gamma("inverse"))
summary(model2)


###Model 1 seems better since it has lower mse.


qqnorm(model1$residuals)
qqline(model1$residuals)





ti<-rstudent(model1)
di<-rstandard(model1)
residuals<-data.frame(ti,di)
filter(residuals,abs(residuals$ti)>2)
filter(residuals,abs(residuals$di)>2)
###Seems there are many possible outlier values. 

###Model Validation: Going to split data into 2 groups (test and train) to see whether the model works well or not.

id <- sample(1:nrow(health), nrow(health)*0.8)
train <- health[id,]
test <- health[-id,]


model2.train <- glm(sqrt(blood_pressure) ~ max_inflation+bmi, data=train, family = Gamma("inverse"))
predicted <- predict(model2.train, train)
mean(((train$blood_pressure) - predicted)^2)


model2.test <- glm(sqrt(blood_pressure) ~ max_inflation+bmi, data=test, family = Gamma("inverse"))
predicted <- predict(model2.test, test)
mean(((test$blood_pressure) - predicted)^2)
summary(m)



model1.train <- glm(sqrt(blood_pressure) ~ max_inflation+bmi, data=train, family = Gamma("identity"))
predicted <- predict(model1.train, train)
mean(((train$blood_pressure) - predicted)^2)
summary(model1.train)

model1.test <- glm(sqrt(blood_pressure) ~ max_inflation+bmi, data=test, family = Gamma("identity"))
predicted <- predict(model1.test, test)
mean(((test$blood_pressure) - predicted)^2)
summary(model1.test)



###R-squared value
with(summary(model1), 1 - deviance/null.deviance)

summary(model1)

###Since mse of the models from train and test data are closer to eachother, we can conclude that our model works well.

###Goodness of fit test.
deviance(model1)
summary(model1)

deviance(model2)
summary(model2)

###Not greater than 1, so both of them are good fit.

###Conclusion: max_inflation and bmi are significantly correlated with the blood pressure according to the generilized linear model.Which means their coefficents are not equal to zero. Around 77.5% of the variability in the blood pressure can be explained by these predictors.  



#######Revised RQ

###Is there any significant blood pressure difference by sex?
###Methods: Wilcox test.
### H0:Means are equal to eachother
### H1:Means are not equal to eachother

health %>%
  ggplot(., aes(x = sex, y = blood_pressure)) +
  geom_violin(trim = F) +
  geom_boxplot(width = 0.2, fill = "beige") + 
  labs(title = "Violin Plot of Blood Pressure by Sex")

with(health, shapiro.test(sqrt(health$blood_pressure[sex == 1])))
with(health, shapiro.test(sqrt(health$blood_pressure[sex == 2])))
###Groups are not follow normal distribution, however according to the assumption of unpaired t-test, they should be distributed normally, so this method cannot be applied. Nonparametric approach can be used instead of t-test which is wilcox test.

wilcox.test(health$blood_pressure~health$sex)

###Since the p value is smaller than the alpha=0.05, we can conclude that there is a significant blood pressure difference between different genders.


###### RQ3 : What is the association among pulse and dietary substances? #####
# response : pulse
# proposed method : variable selection, logistic regression

###### RQ4 : Is there an association between the total number of people in the household and protein? ####
# response : protein
# proposed method : Anova, Tukey

### H0: No difference mean
### H1: At least one of them is different
###NOTE! We are assuming the assumptions of anova which are normality, homogenity of variances and independency satisfied. If not, applying anova cannot be possible. Non-parametric approach to anova is kruskal-wallis test.

boxplot(health$protein~ health$total_num_people_household,
        main = "Distribution of protein by number of people in the household",
        xlab = "number of people",
        ylab = "protein")


table(test$total_num_people_household)
table(train$total_num_people_household)

prop.table(table(test$total_num_people_household))
prop.table(table(train$total_num_people_household))

aov.model<- aov((protein) ~ as.factor(total_num_people_household),data=health)
summary(aov.model)


TukeyHSD(aov.model, conf.level=.95) 
plot(TukeyHSD(model4, conf.level=.95), las = 2)

###According to the plot above and p-values, It can be concluded that there is a significant difference between the groups 3-2 and 5-2 which indicates significant protein level difference between houses with 2 people and 3,5 people.



health <- read.csv("C:/Users/90551/Desktop/SONDÖNEM/stat364/Stat364-Proje/nhanes_systolic.csv", header = T, sep = ";")
health
str(health)
summary(health)





colnames(health) <- c("resp_seq_num", "total_num_people_household", "total_household_income", "sex", "age", "blood_pressure", "bmi", "pulse", "max_inflation",
                      "alcohol", "sodium", "potassium", "calcium", "magnesium", "protein", "energy", "smoking"  )
str(health)
summary(health)
sum(is.na(health))
sum(is.null(health))

health$sex <- as.factor(health$sex)
health$pulse <- as.factor(health$pulse)
health$smoking <- as.factor(health$smoking)

health$bmi <- gsub(",", ".", health$bmi)
health$alcohol <- gsub(",", ".", health$alcohol)
health$protein <- gsub(",", ".", health$protein)

health$alcohol <- as.numeric(health$alcohol)
health$bmi <- as.numeric(health$bmi)
health$protein <- as.numeric(health$protein)

str(health)
summary(health)
sum(is.na(health))
sum(is.null(health))
range(health$total_num_people_household)
range(health$total_household_income)




################3333333333

head(health$pulse)
health$pulse = ifelse(health$pulse == 1, 0, 1)

# RQ3: What is the association among pulse and other variables? 


#model1data=health[,c(10:16,8)]

library(caret)#its a classification and regression training



set.seed(123) # setting seed to generate a reproducible random sampling
# creating training data as 80% of the dataset
random_sample <- createDataPartition(health$pulse, p = 0.8, list = FALSE)
train  <- health[random_sample, ] # generating training dataset from the random_sample
test <- health[-random_sample, ] # generating testing dataset from rows which are not included in random_sample



prop.table(table(train$pulse))
prop.table(table(test$pulse))

#the proportion of the levels of the response are close to each other. Now, let us fit the model with Logistic regression

summary(train)
cor(train[,c(1:3,5,6,7,9:16)])

#there can be relation btw potasium and sodium , corr is 0.64041552

library(olsrr)


health.model<- glm(pulse ~ ., data=train, family="binomial")
summary(health.model)
##########################333

library(MASS)
step.model_1 <- health.model %>% stepAIC(trace = FALSE)
coef(step.model_1)



############+444444

#then by using significant variable new model was createed.Also, by using stepwise the best model was found
hm<- glm(pulse ~ sex+age, data=train, family="binomial")
summary(hm)

#to get coeff:
hm$coefficients




#to interpret odd ratio

exp(hm$coefficients)

# Odds ratio and 95% CI
exp(cbind(OR = coef(hm), confint(hm)))
#Interp. odds ratios:

library(car)

vif(hm)  # all vif are less than 5 ,there is no multi collinearity.
install.packages("cutpointr")

library(cutpointr)

#for prediction step :
library("AER")


pred.probabilities <- hm %>% predict(test, type = "response")
head(pred.probabilities)


# this is the predicted score. these scores are between 0 and 1
contrasts(test$pulse)
install.packages("cutoff")
install.packages("InformationValue")
library(InformationValue)
library(cutpointr)
library(tidyverse)

library(dplyr)
str(test)

#tekrar bak optimalCutoff fnc library :
optCutOff2 <- optimalCutoff(test$pulse, pred.probabilities)[1]   #0.007950483
pred.test2 = ifelse(pred.probabilities > optCutOff2, 1, 0)
head(pred.test2)

test_tab2 = table(predicted = pred.test2,actual = test$pulse)
accuracy2<-(test_tab2[1, 1] + test_tab2[2, 2]) / sum(test_tab2)
library(caret)
sensitivity2 <- sensitivity (test$pulse, as.factor(pred.test2))
specificity2<-specificity(as.factor(test$pulse), as.factor(pred.test2))
data.frame(accuracy2,sensitivity2,specificity2)


predicted.classes_0.2 <- ifelse(pred.probabilities > optCutOff2, "1", "0")
#Creating confusion matrix
measeures_02 <- confusionMatrix(data=factor(pred.test2),
                                reference = factor(test$pulse))

library(tidyverse)
library(caret)
library(mlbench)
library(caret)
library(InformationValue)
library(ISLR)
confusionMatrix(test$pulse, pred.probabilities)
sensitivity(test$pulse, pred.probabilities)
specificity(test$pulse, pred.probabilities)
misClassError(test$pulse, pred.probabilities, threshold=optCutOff2)
