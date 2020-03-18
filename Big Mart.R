#set the working directory
setwd('E:\\missed classes\\bigmart')
#import train and test data
train<-read.csv('train_kOBLwZA.csv', header = TRUE, sep =',' ,
                na.strings = c("NA","NaN"," "))
test<-read.csv('test_t02dQwI.csv', header = TRUE, sep = ',', 
               na.strings = c("NA","NaN"," "))
#audit the data
str(train)
#Summary
summary(train)
#imputation of missing value
train$Item_Weight<-ifelse(is.na(train$Item_Weight),#condition
                          median(train$Item_Weight, na.rm=TRUE),#function
                          train$Item_Weight)#else part
sum(is.na(train$Item_Weight))
#Solve inconsistency 
train$Item_Fat_Content<-as.character(train$Item_Fat_Content)
is.character(train$Item_Fat_Content)
train$Item_Fat_Content<-ifelse(train$Item_Fat_Content=='LF',
                               'Low Fat',
                               train$Item_Fat_Content)
train$Item_Fat_Content<-ifelse(train$Item_Fat_Content=='low fat',
                               'Low Fat',
                               train$Item_Fat_Content)
train$Item_Fat_Content<-ifelse(train$Item_Fat_Content=='reg',
                               'Regular',
                               train$Item_Fat_Content)
str(train$Item_Fat_Content)
summary(train$Item_Fat_Content)
summary(train)
table(train$Item_Fat_Content)
train$Item_Fat_Content<-as.factor(train$Item_Fat_Content)
summary(train$Item_Fat_Content)
summary(train)
str(train$Outlet_Size)
train$Outlet_Size<-as.character(train$Outlet_Size)
train$Outlet_Size<-ifelse(train$Outlet_Size=="",
                          'Medium',
                          train$Outlet_Size)
summary.factor(train$Outlet_Size)
train$Outlet_Size<-as.factor(train$Outlet_Size)
str(train$Outlet_Size)
summary(train$Outlet_Size)
summary.factor(train$Outlet_Size)
summary(train)
#skewness 
library(e1071)
skewness(train)#error bcoz its only for continuos variable

skewness(train$Item_Weight)
boxplot(train$Item_Weight)
skewness(train$Item_Visibility)
skewness(train$Item_MRP)
skewness(train$Item_Outlet_Sales)
boxplot(train$Item_Outlet_Sales)
table(train$Outlet_Size,train$Outlet_Location_Type)
#to check the sample is good or not
#for categorical value , we do chi test
#since value is contadic to our assumption , we go for chi test
#check for P value
table(train$Item_Fat_Content,train$Item_Type)
install.packages('MASS')
library(MASS)
table1<-table(train$Outlet_Size,train$Outlet_Location_Type)
chisq.test(table1)
chisq.test(table(train$Item_Fat_Content,train$Item_Type))
chisq.test(table(train$Outlet_Location_Type,train$Outlet_Type))
#uni varinet-- mean median mode skewness summary str
#bi varient -- correlation to check dependency
#if value is towards 1, then there is a relationship
#if towards 0 no relationship
cor(train$Item_Weight,train$Item_Visibility)
cor(train$Item_Weight,train$Item_MRP)
cor(train$Item_Visibility,train$Item_MRP)
cor(train$Item_MRP,train$Item_Outlet_Sales)
#multi varient, b4 dng we need to reduce skewness

install.packages('Hmisc')
library(Hmisc)
describe(train)
summary(train)

plot(train$Item_MRP,train$Item_Outlet_Sales)
plot(train$Item_Weight,train$Item_MRP)
plot(train$Item_Visibility,train$Item_MRP)

train$Ln_Item_Weight<-log(train$Item_Weight)
train$Ln_Item_Weight
summary(train$Ln_Item_Weight)
describe(train$Ln_Item_Weight)
skewness(train$Ln_Item_Weight)
train$Ln_sales<-log(train$Item_Outlet_Sales)
skewness(train$Item_Outlet_Sales)
skewness(train$Ln_sales)
train$ln_visibility<-log(train$Item_Visibility)
skewness(train$Item_Visibility)
skewness(train$ln_visibility)
str(train$Item_Visibility)
train$scale_weighgt<-scale(train$Item_Weight)
skewness(train$Item_Weight)
skewness(train$scale_weighgt)
train$scale_outletsale<-scale(train$Item_Outlet_Sales)
skewness(train$Item_Outlet_Sales)
skewness(train$scale_outletsale)
library(e1071)
#creating new variable
train$yob<-2019-train$Outlet_Establishment_Year
train$yob

names(train)
#apply regression
model<-lm(Item_Outlet_Sales~Item_Weight+Item_Visibility+Item_MRP+yob,data=train)
summary(model)
#predicted sales from the model for train
train$predicted_sale<-predict(model,train)
train$predicted_sale

rmse_model<-sqrt(mean((train$Item_Outlet_Sales-train$predicted_sale)^2))
print(rmse_model)
#apply regressio with new variable
model1<-lm(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+
             Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+yob, data = train)

summary(model1)
#predict sales from model1(includes categorical variable also) for train
train$predicted_sales1<-predict(model1,train)
rmse_model1<-sqrt(mean((train$Item_Outlet_Sales-train$predicted_sales1)^2))
print(rmse_model1)
##preparation of new model#####
names(train)
train$scale_outletsale<- NULL
summary(train)
skewness(train$Item_Weight)
library(e1071)
##log##
train$New_Item_Weight<-log(train$Item_Weight)
train$New_Item_Visibility<-log(train$Item_Visibility)
train$New_Item_MRP<-log(train$Item_MRP)
skewness(train$Item_Weight)
skewness(train$New_Item_Weight)
skewness(train$Item_Visibility)
skewness(train$New_Item_Visibility)
skewness(train$Item_MRP)
skewness(train$New_Item_MRP)
##scale##
train$New1_ItemWeight<-scale(train$Item_Weight)
train$New1_ItemVisibility<-scale(train$Item_Visibility)
train$New1_ItemMRP<-scale(train$Item_MRP)
skewness(train$Item_Weight)
skewness(train$New1_ItemWeight)
skewness(train$Item_Visibility)
skewness(train$New1_ItemVisibility)
skewness(train$Item_MRP)
skewness(train$New1_ItemMRP)
skewness(train$yob)
train$New_yob<-log(train$yob)
skewness(train$New_yob)
train$New1_yob<-scale(train$yob)
skewness(train$New1_yob)
##########preparation of test data###########
str(test)
summary(test)
test$Item_Weight<- ifelse(is.na(test$Item_Weight),
                          median(test$Item_Weight, na.rm=TRUE),
                          test$Item_Weight)
summary(test$Item_Weight)
test$Item_Fat_Content<-as.character(test$Item_Fat_Content)
is.character(test$Item_Fat_Content)
test$Item_Fat_Content<-ifelse(test$Item_Fat_Content=='LF',
                              'Low Fat',
                              test$Item_Fat_Content)
test$Item_Fat_Content<-ifelse(test$Item_Fat_Content=='low fat',
                              'Low Fat',
                              test$Item_Fat_Content)
test$Item_Fat_Content<-ifelse(test$Item_Fat_Content=='reg',
                              'Regular',
                              test$Item_Fat_Content)
test$Item_Fat_Content<-as.factor(test$Item_Fat_Content)
summary(test)
test$Outlet_Size<-as.character(test$Outlet_Size)
test$Outlet_Size<-ifelse(test$Outlet_Size=="",
                         'Medium',
                         test$Outlet_Size)
summary.factor(test$Outlet_Size)
test$Outlet_Size<-as.factor(test$Outlet_Size)
summary(test)
test$yob<-2019-test$Outlet_Establishment_Year
#we cant create model so use the model from train#
test$Item_Outlet_Sales<-predict(model1,test)

#write test to csv file##
write.csv(test,'Submit1.csv')
getwd()
summary(model)
names(test)

submission<-c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales')
submit <- test[submission]
write.csv(submit, 'submission.csv', row.names = FALSE)

str(submission)
str(submit)
summary(test)


#####randomforest####

library(randomForest)
model_randomforest<-randomForest(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+
             Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+yob, data = train,ntree=150)

print(model_randomforesttest150)
summary(model_randomforest)

train$predicted_randomforest <- predict(model_randomforest,train)

rmse_model1<-sqrt(mean((train$Item_Outlet_Sales-train$predicted_randomforest)^2))


print(rmse_model1)

#####decision tree#####
library(rpart)

model_decisiontree<-rpart(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+
                                   Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+yob, data = train,parms=list(split='information'))
train$predicted_decisiontree<- predict(model_decisiontree,train)

rmse_model2<-sqrt(mean((train$Item_Outlet_Sales-train$predicted_decisiontree)^2))
###calculation of rmse with decision tree,randomforest,ln####
#rmse_model <- ln model
#rmse_model1 <- randomforest
#rmse_model2 <- decisontree
print(rmse_model2)
print(rmse_model1)
print(rmse_model)

###do same for test data set####

test$Item_Outlet_Sales_decisiontree<-predict(model_decisiontree,test)
test$Item_Outlet_Sales_randomforest<-predict(model_randomforest,test)

###summission file###

submission<-c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales')
submit <- test[submission]
write.csv(submit, 'submission.csv', row.names = FALSE)
submission<-c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales_decisiontree')
submit <- test[submission]
write.csv(submit, 'submissiondecision.csv', row.names = FALSE)
submission<-c('Item_Identifier','Outlet_Identifier','Item_Outlet_Sales_randomforest')
submit <- test[submission]
write.csv(submit, 'submissionrandomforest.csv', row.names = FALSE)


####tuning a model####

model_randomforesttest<-randomForest(Item_Outlet_Sales~Item_Weight+Item_Fat_Content+Item_Visibility+
                                   Item_Type+Item_MRP+Outlet_Size+Outlet_Location_Type+Outlet_Type+yob, data = train,ntree=200)

#print(model_randomforesttest150)
#summary(model_randomforest)

train$predicted_randomforesttest <- predict(model_randomforesttest,train)

rmse_modeltest<-sqrt(mean((train$Item_Outlet_Sales-train$predicted_randomforesttest)^2))
rmse_modeltest

install.packages("rpart.plot")
library("rpart.plot")

rpart.plot(model_decisiontree)

train$ensamble <- (train$predicted_sales1+train$predicted_decisiontree+train$predicted_randomforest)/3
rmse_model<-sqrt(mean((train$Item_Outlet_Sales-train$ensamble)^2))
rmse_model


test$ensamble <- (test$Item_Outlet_Sales+test$Item_Outlet_Sales_decisiontree+test$Item_Outlet_Sales_randomforest)/3

submission<-c('Item_Identifier','Outlet_Identifier','ensamble')
submit <- test[submission]
write.csv(submit, 'submissionensamble.csv', row.names = FALSE)
