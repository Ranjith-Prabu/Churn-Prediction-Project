#CHURN PREDICTION (TELCOM SECTOR)


#Setting working directory
setwd("C:/Users/Ranjith P/Desktop")
getwd()


#Loading the train and test dataset:
train_data=read.csv("C:/Users/Ranjith P/Desktop/Train_data.csv")
test_data=read.csv("C:/Users/Ranjith P/Desktop/Test_data.csv")


#Removing 3 variables named "state","area code","phone number"; 
#which are not given as predictors in data dictionary
train=train_data[,-c(1,3,4)]
dim(train)


test=test_data[,-c(1,3,4)]
dim(test)


#Installing Packages:
#install.packages("MASS","DMwR","ggplot2","purrr","tidyr","corrgram","caret","randomForest","RRF")


#Loading the libraries:
library(MASS)
library(DMwR)
library(plyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(corrgram)
library(caret)
library(randomForest)
library(RRF)


#Exploratory  Data Analysis of Train Data:
summary(train)
dim(train)
str(train)


#Data Cleaning & Data Preparation:


#Missing value Analysis and Treatment:
mv=data.frame(apply(train, 2, function(x){sum(is.na(x))}))
mv
#It's found that there is no missing value in any variable in our dataset.


#Data Visualization (EDA):

#Here Target variable is Churn
#Target class proportion:
ggplot(train,aes(train$Churn))+geom_bar(aes(fill = train$Churn),position = "dodge")+
  labs(title = "Churn vs No Churn")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=0.5))


#Plotting categorical variable"international.plan" and Target:
ggplot(train,aes(x=train$international.plan,fill=train$Churn))+
  geom_bar(position="dodge")+labs(title = "Intl Plan vs Churn")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Plotting categorical variable"train$voice.mail.plan" and Target:
ggplot(train,aes(x=train$voice.mail.plan,fill=train$Churn))+
  geom_bar(position="dodge")+labs(title = "Voice mail plan vs Churn")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)


#Plotting number.customer.service.calls and Target:
ggplot(train,aes(x=factor(train$number.customer.service.calls),fill=train$Churn))+
  geom_bar()+labs(title = "Customer Service calls vs Churn")+
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))


#Plotting all numerical variables:
train %>%keep(is.numeric) %>% gather() %>%ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +geom_histogram()+
  labs(title = "Distribution of Numerical variables")


#Plotting some numerical variables with Target variable in order to understand the nature of data:

ggplot(train, aes(x = train$account.length))+
  geom_histogram(aes(color = train$Churn), fill = "white", position = "identity") +
  scale_color_manual(values = c("red", "blue"))+labs(title = "account length vs churn")


ggplot(train, aes(x = train$number.vmail.messages))+
  geom_histogram(aes(color = train$Churn), fill = "white", position = "identity") +
  scale_color_manual(values = c("red", "blue"))+labs(title = "vmail messages vs churn")


ggplot(train, aes(x = train$total.day.minutes))+
  geom_histogram(aes(color = train$Churn), fill = "white", position = "identity") +
  scale_color_manual(values = c("red", "blue"))+labs(title = "total day minutes vs churn")


ggplot(train, aes(x = train$total.eve.minutes))+
  geom_histogram(aes(color = train$Churn), fill = "white", position = "identity") +
  scale_color_manual(values = c("red", "blue"))+labs(title = "evening minutes vs churn")


ggplot(train, aes(x = train$total.night.minutes))+
  geom_histogram(aes(color = train$Churn), fill = "white", position = "identity") +
  scale_color_manual(values = c("red", "blue"))+labs(title = "total night minutes vs churn")


ggplot(train, aes(x = train$total.intl.charge))+
  geom_histogram(aes(color = train$Churn), fill = "white", position = "identity") +
  scale_color_manual(values = c("red", "blue"))+labs(title = "total intl minutes vs churn")


ggplot(train, aes(x = train$number.customer.service.calls))+
  geom_histogram(aes(color = train$Churn), fill = "white", position = "identity") +
  scale_color_manual(values = c("red", "blue"))+labs(title = "No. of Customer Service calls vs Churn")

#We can change the x variables here to view the plots of other independent numeric variables.#Also, it's evident that most of the variables are normally distributed.


#Outlier Analysis and Treatment:
#Taking only the continuous variables to deal with outliers.
numerics=sapply(train, is.numeric)
numeric_train=train[,numerics]
train_numeric_names=colnames(numeric_train)
train_numeric_names


#Visulaizing the outliers using boxplot:
boxplot(numeric_train)


#Plotting the variables seperately based on Target:
for(i in 1:length(train_numeric_names))
{
  assign(paste0("Train",i),ggplot(aes_string(y = (train_numeric_names[i]),x="Churn"),data = subset(train))+
           stat_boxplot(geom = "Boxplot",width = 0.5)+
           geom_boxplot(outlier.colour = "red",fill = "green",outlier.shape = 20,
                        outlier.size = 2,notch = FALSE)+
           theme(legend.position = "Top")+
           labs(y=train_numeric_names[i],x="Churn")+
           ggtitle(paste("Boxplot Visualization",train_numeric_names[i])))
}
gridExtra::grid.arrange(Train1,Train2,Train3,ncol = 3)
gridExtra::grid.arrange(Train4,Train5,Train6,ncol = 3)
gridExtra::grid.arrange(Train7,Train8,Train9,ncol =3)
gridExtra::grid.arrange(Train10,Train11,Train12,ncol = 3)
gridExtra::grid.arrange(Train13,Train14,Train15,ncol = 3)



#Since the no.of observations in train dataset are just 3333, I'm not removing the outliers. 
#Instead I'm imputing the outliers using KNN.


#Changing the outlier values to NA's
for(i in train_numeric_names){
  outlier_value=train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  train[,i][train[,i] %in% outlier_value]=NA
}


sum(is.na(train))
mv1=data.frame(apply(train, 2, function(x){sum(is.na(x))}))
mv1
#Thus the outlier values have been changed as NA's


#Imputing NA's using KNN Algorithm
train1=knnImputation(train,k=3)
sum(is.na(train1))
#Thus treated the NA's using KNN


#Boxplot visualization after imputing outliers:
boxplot(train[,-c(2,3,18)])


#Transforming the categorical variables:
#Changing the target to 0=False. 1=True.
summary(train1$Churn)
unique(train1$Churn)# Identifying the no. of levels
train1$Churn=factor(train1$Churn,levels = c(" False.", " True."),labels = c(0,1))


#Changing the categorical independent variables in to factor. Normally we will create dummy variables. 
#Since there are only two levels in both categorical variables, I'm performing the following.
unique(train1$international.plan) # Identifying the no. of levels
train1$international.plan=as.integer(train1$international.plan)


unique(train1$voice.mail.plan) # Identifying the no. of unique levels
train1$voice.mail.plan=as.integer(train1$voice.mail.plan)


#Feature Selection:
str(train1)
summary(train1)


#Correlation to check for multicollinearity among independent variables:


#Correlation Plot:
corrgram(train1[,],order=FALSE,upper.panel=panel.pie,text.panel=panel.txt,main="correlation plot")
#The chart shows that some variables are highly correlated. So, we drop variables to get rid of multicollinearity.


colnames(train1)
train1=subset(train1,select=-c(voice.mail.plan,total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes))
#Thus removed the multicollineared varibles.


corrgram(train1[,],order=FALSE,upper.panel=panel.pie,text.panel=panel.txt,main="correlation plot")
#Thus there is no multicollinearity problem in the dataset.


#Feature Scaling:
range(train1$account.length)


#Ploting all predictors together in histogram:
train1 %>%keep(is.numeric) %>% gather() %>%ggplot(aes(value)) +facet_wrap(~ key, scales = "free") +geom_histogram()


#The above graph shows that some variables such as "account.length","total.day.calls","total.day.charge",
#"total.eve.calls","total.eve.charge","total.intl.charge","total.night.calls","total.night.charge" are mostly normally distributed. So, we perform standardisation on it.


#Other variables such as "number.customer.service.calls","number.vmail.messages","total.intl.calls" are skewed. 
#So, I perform normalization on these variables.


#Standardisation:
train_normal_names=c("account.length","total.day.calls","total.day.charge","total.eve.calls","total.eve.charge","total.intl.charge","total.night.calls","total.night.charge")


for(i in train_normal_names){
  print(i)
  train1[,i]=(train1[,i]-mean(train1[,i]))/sd(train1[,i])
}


range(train1$account.length)
sum(is.na(train1))


#Normalisation:
train_skew_names=c("number.customer.service.calls","number.vmail.messages","total.intl.calls" )


for (i  in train_skew_names){
  print(i)
  train1[,i]=(train1[,i]-min(train1[,i]))/(max(train1[,i]-min(train1[,i])))
}


summary(train1)



#Saving final training dataframe as final
final=train1


#################################################################################################################3
#Test data:


#Exploratory Data Analysis of Test Data:
summary(test)
dim(test)
str(test)


#Missing value Analysis and Treatment on Test Data:
mvt=data.frame(apply(test, 2, function(x){sum(is.na(x))}))
mvt


#Visualising outliers:
#Taking only the continuous variables to deal with outliers.
test_numerics_index=sapply(test, is.numeric)
test_numerics=test[,test_numerics_index]
#test_numeric_names=colnames(test_numerics)
#test_numeric_names
boxplot(test_numerics)



#Transforming the categorical variables:
summary(test$Churn)
test$Churn<- factor(test$Churn,levels = c(" False.", " True."),labels = c(0,1))


#test$international.plan <- factor(test$international.plan,levels = c(" no", " yes"),labels = c(0,1))
test$international.plan=as.integer(test$international.plan)
summary(test$international.plan)
test$voice.mail.plan=as.integer(test$voice.mail.plan)
summary(test$voice.mail.plan)


str(test)


#Feature Selection:
corrgram(test[,],order=FALSE,upper.panel=panel.pie,text.panel=panel.txt,main="correlation plot")


#Removing multicollineared variables:(These were removed in train data. So, removing in teat data also.)
test1=subset(test,select=-c(voice.mail.plan,total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes))


corrgram(test1[,],order=FALSE,upper.panel=panel.pie,text.panel=panel.txt,main="correlation plot")


#Feature Scaling:
test1 %>%keep(is.numeric) %>% gather() %>%ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +geom_histogram()+
  labs(title = "Distribution of Test Data")


#Standardisation:
test_normal_names=c("account.length","total.day.calls","total.day.charge","total.eve.calls","total.eve.charge","total.intl.charge","total.night.calls","total.night.charge")
for(i in test_normal_names){
  print(i)
  test1[,i]=(test1[,i]-mean(test1[,i]))/sd(test1[,i])
}


#Normalisation
test_skew_names=c("number.customer.service.calls","number.vmail.messages","total.intl.calls" )
for (i  in test_skew_names){
  print(i)
  test1[,i]=(test1[,i]-min(test1[,i]))/(max(test1[,i]-min(test1[,i])))
}


summary(test1)


#Removing target variable form test data seperately.
test2=test1[,-13]

summary(test2)


####################################################################################################################
#Model Building and Prediction:
#Logistic Regression:

model1=glm(Churn~.,data=final,family="binomial")
summary(model1)


#Step AIC to remove insignificant variables:
stepAIC(model1)
model2=glm(Churn ~ international.plan + number.vmail.messages + 
             total.day.charge + total.eve.charge + total.night.charge + 
             total.intl.calls + total.intl.charge, family = "binomial", 
             data = final)
summary(model2)


model2$coefficients


prediction_glm=round(predict(model2,newdata =test2,type="response"),2)
prediction_glm


#Setting threshold value:
#prediction_glm=ifelse(prediction_glm>0.5,1,0)


compare_glm=table(test1$Churn,round(prediction_glm))
compare_glm


#Confusion matrix for Logistic Regression:
confusionMatrix(compare_glm)


#FNR:(~)
190/(190+34)
#FNR: 84% which is very high. So, need to try with other model.
#Accuracy: 87%


#Creating Data Frame of actual and predicted values:
df_glm=data.frame(test_data$phone.number,test1$Churn,round(prediction_glm))
df_glm$round.prediction_glm.=as.factor(df_glm$round.prediction_glm.)
View(df_glm)


#Logistic Regression:   Accuracy=87%     FNR= 84%  (Need to reduce the FNR)



#Random Forest:
rf_model1=randomForest(Churn~.,train1,importance=TRUE,ntree=500)
rf_model1
summary(rf_model1)


# Fine tuning parameters of Random Forest model (ie., changing mtry value to 9)
rf_model2<- randomForest(Churn ~ ., data=train1, ntree = 500, mtry = 9, keep.forest=TRUE,importance = TRUE)
rf_model2
#Thus the error rate is reduced.


prediction_rf=predict(rf_model2,test2)
prediction_rf


compare_rf=table(test1$Churn,prediction_rf)
compare_rf


#Confusion matrix for Random Forest:
confusionMatrix(compare_rf)


#FNR: (~may change slightly)
75/(75+149)
#FNR ~:33% (ok)
#Accuracy:88%


#Creating dataframe of actual and predicted values:
df_rf=data.frame(test_data$phone.number,test1$Churn,prediction_rf)
df_rf$prediction_rf=as.factor(df_rf$prediction_rf)
View(df_rf)



#Variable Importances from Random Forest:
varImp(rf_model2)
rf_model2$importance

#Random Forest:       Accuracy=88%     FNR=33%   (Model is optimal)



#Data Frame of prediction using both algorithms and actual values:
df=data.frame(test_data$phone.number,test1$Churn,round(prediction_glm),prediction_rf)
summary(df)


#########################################################################################################


