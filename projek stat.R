library(readxl)
data <- read_excel("Marketingcampaigns.xlsx")
head(data)
str(data)
is.na(data)
summary(data)
data <- read_excel("Marketingcampaigns.xlsx")
head(data)
str(data)
age<-data$Age
gender<-data$Gender
openemail<-data$`Email Opened`
clickemail<-data$`Email Clicked`
discount<-data$`Discount offered`
purchased<-data$Purchased
datanew<-data.frame(purchased,age,gender,openemail,clickemail,discount)
summary(age)
summary(gender)
summary(openemail)
summary(clickemail)
summary(discount)
summary(purchased)
xtabs(~ purchased+age, data = datanew)
xtabs(~ purchased+gender, data = datanew)
xtabs(~ purchased+openemail, data = datanew)
xtabs(~ purchased+clickemail, data = datanew)
xtabs(~ purchased+discount, data = datanew)
mydata.cor<-cor(datanew)
library(corrplot)
corrplot(mydata.cor)
library(readr)
library(caret)
inTrain <- createDataPartition(y = datanew$purchased, p = .80, list = FALSE)
training <- datanew[inTrain,]
testing <- datanew[-inTrain,]
hipermodel21 <-glm(purchased ~ age+gender+openemail+clickemail+discount,data=training,family=binomial)
summary(hipermodel21)
library(lmtest)
lrtest(hipermodel21)
hipermodel21 <-glm(purchased ~ age+gender+openemail+clickemail+discount,data=training,family=binomial)
hipermodel22 <-glm(purchased ~ age+gender+openemail+clickemail,data=training,family=binomial)
hipermodel23 <-glm(purchased ~ age+gender+openemail,data=training,family=binomial)
hipermodel24 <-glm(purchased ~ age+gender,data=training,family=binomial)
hipermodel25 <-glm(purchased ~ age,data=training,family=binomial)
hipermodel21$aic
hipermodel22$aic
hipermodel23$aic
hipermodel24$aic
hipermodel25$aic
hipermodel21prob <-glm(purchased ~ age+gender+openemail+clickemail+discount,
                       data=training,family=binomial(link="probit"))
summary(hipermodel21prob)
library(lmtest)
lrtest(hipermodel21prob)
hipermodel21prob <-glm(purchased ~ age+gender+openemail+clickemail+discount,
                       data=training,family=binomial(link="probit"))
hipermodel22prob <-glm(purchased ~ age+gender+openemail+clickemail,
                       data=training,family=binomial(link="probit"))
hipermodel23prob <-glm(purchased ~ age+gender+openemail,
                       data=training,family=binomial(link="probit"))
hipermodel24prob <-glm(purchased ~ age+gender,
                       data=training,family=binomial(link="probit"))
hipermodel25prob <-glm(purchased ~ age,
                       data=training,family=binomial(link="probit"))
hipermodel21prob$aic
hipermodel22prob$aic
hipermodel23prob$aic
hipermodel24prob$aic
hipermodel25prob$aic
summary(hipermodel25)
hip.prob = predict(hipermodel26prob, testing, type="response")
y_pred_num <- ifelse(hip.prob > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testing$purchased
table(y_pred, y_act)
mean(y_pred == y_act)
1-mean(y_pred == y_act)