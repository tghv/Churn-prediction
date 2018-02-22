library(sqldf)
library(DMwR)
library(ggplot2)
library(randomForest)
library(reshape2)
library(caret)
library(devtools)
library(MASS)
library(e1071)
library(ROSE)
library(rpart)
library(C50)
library(devtools)
library(plyr)
library(dummies)
library(dplyr)
library(naivebayes)
library(MlBayesOpt)
library()




dummy.data.frame()
#Setting the wd
setwd("~/PHD hackathon/TrainData")
train1<-read.csv("Train.csv")
train2<-read.csv("Train_AccountInfo.csv")
train3<-read.csv("Train_Demographics.csv")
train4<-read.csv("Train_ServicesOptedFor.csv")
train5<-dcast(train4, CustomerID ~ TypeOfService)

#Setting the wd
setwd("~/PHD hackathon/TestData")
test1<-read.csv("Test.csv",na.strings="?")
test2<-read.csv("Test_AccountInfo.csv",na.strings="?")
test3<-read.csv("Test_Demographics.csv",na.strings="?")
test4<-read.csv("Test_ServicesOptedFor.csv",na.strings="?")
test5<-dcast(test4, CustomerID ~ TypeOfService)

train3$HouseholdID

d1<-merge(train1,train2)
d2<-merge(d1,train3)
d3<-merge(d2,train5)
new_train_data<-d3




e1<-merge(test1,test2)
e2<-merge(e1,test3,by.x ="CustomerID",by.y ="HouseholdID")
e3<-merge(e2,test5)


target_id<-subset(e3,select=c(CustomerID))
head(target_id)

new_test_data<-e2

# write.csv(new_train_data,"new_train_data.csv",row.names = F)
# write.csv(new_test_data,"new_test_data.csv",row.names = F)
# 
# test_cust_id<-read.csv("new_test_data.csv")

# 
# setwd("~/PHD hackathon")
# new_train_data<-read.csv("new_train_data.csv")
# new_test_data<-read.csv("new_test_data.csv")


#target_id<-subset(test_cust_id,select=c(16))




#Exploring the data
head(new_train_data)
tail(new_train_data)
head(new_test_data)
tail(new_test_data)
str(new_train_data)
#Dimensions of data
dim(new_train_data)
dim(new_test_data)


#Datatypes of variables
str(new_train_data)
str(train_data)

#Data given is from maharastra state
#So nullfying state and country as they have no change
table(test_data$State)
table(train_data$State)

new_train_data$State<-NULL
new_test_data$State<-NULL

new_train_data$Country<-NULL
new_test_data$Country<-NULL

#Cust Id should be removed
new_train_data$CustomerID<-NULL
new_test_data$CustomerID<-NULL


#Date is nullified
new_train_data$DOE<-NULL
new_test_data$DOE<-NULL

new_train_data$DOC<-NULL
new_test_data$DOC<-NULL



#Total charges should be integer type
new_train_data$TotalCharges<-as.numeric(new_train_data$TotalCharges)
new_test_data$TotalCharges<-as.numeric(new_test_data$TotalCharges)



new_train_data[new_train_data=="?"]<-NA
new_train_data[new_train_data=="NA"]<-NA
new_train_data[new_train_data==""]<-NA
new_train_data[new_train_data=="MISSINGVAL"]<-NA
new_train_data[new_train_data==" "]<-NA



new_test_data[new_test_data=="?"]<-NA
new_test_data[new_test_data=="NA"]<-NA
new_test_data[new_test_data==""]<-NA
new_test_data[new_test_data=="MISSINGVAL"]<-NA
new_test_data[new_test_data==" "]<-NA


str(new_test_data)


#Replacement
new_train_data$HasPhoneService<-as.factor(mapvalues(new_train_data$HasPhoneService,from=c("0","1"),to=c("No","Yes")))
new_train_data$Retired<-as.factor(mapvalues(new_train_data$Retired,from=c("0","1"),to=c("No","Yes")))
new_train_data$HasPartner<-as.factor(mapvalues(new_train_data$HasPartner,from=c("1","2"),to=c("Yes","No")))
new_train_data$HasDependents<-as.factor(mapvalues(new_train_data$HasDependents,from=c("1","2"),to=c("Yes","No")))

new_test_data$HasPhoneService<-as.factor(mapvalues(new_test_data$HasPhoneService,from=c("0","1"),to=c("No","Yes")))
new_test_data$Retired<-as.factor(mapvalues(new_test_data$Retired,from=c("0","1"),to=c("No","Yes")))
new_test_data$HasPartner<-as.factor(mapvalues(new_test_data$HasPartner,from=c("1","2"),to=c("Yes","No")))
new_test_data$HasDependents<-as.factor(mapvalues(new_test_data$HasDependents,from=c("1","2"),to=c("Yes","No")))


#Factor train conversion
new_train_data$DeviceProtection<-as.factor(new_train_data$DeviceProtection)
new_train_data$InternetServiceCategory<-as.factor(new_train_data$InternetServiceCategory)
new_train_data$MultipleLines<-as.factor(new_train_data$MultipleLines)
new_train_data$OnlineBackup<-as.factor(new_train_data$OnlineBackup)
new_train_data$OnlineSecurity<-as.factor(new_train_data$OnlineSecurity)
new_train_data$StreamingMovies<-as.factor(new_train_data$StreamingMovies)
new_train_data$StreamingTelevision<-as.factor(new_train_data$StreamingTelevision)
new_train_data$TechnicalSupport<-as.factor(new_train_data$TechnicalSupport)
new_train_data$TotalCharges<-as.numeric(new_train_data$TotalCharges)

#Factor test conversion

new_test_data$DeviceProtection<-as.factor(new_test_data$DeviceProtection)
new_test_data$InternetServiceCategory<-as.factor(new_test_data$InternetServiceCategory)
new_test_data$MultipleLines<-as.factor(new_test_data$MultipleLines)
new_test_data$OnlineBackup<-as.factor(new_test_data$OnlineBackup)
new_test_data$OnlineSecurity<-as.factor(new_test_data$OnlineSecurity)
new_test_data$StreamingMovies<-as.factor(new_test_data$StreamingMovies)
new_test_data$StreamingTelevision<-as.factor(new_test_data$StreamingTelevision)
new_test_data$TechnicalSupport<-as.factor(new_test_data$TechnicalSupport)
new_test_data$TotalCharges<-as.numeric(new_test_data$TotalCharges)








#Changing the no internet service level

# str(new_train_data)
# colnames(new_train_data)
# cols<- c(1,5:9)
# for(i in 1:ncol(new_train_data[,cols])) {
#   new_train_data[,cols][,i] <- as.factor(mapvalues
#                                          (new_train_data[,cols][,i], from =c("No internet service"),to=c("No")))
#  }
# 
# str(new_test_data)
# 
# 
# test_cols<-c(11,15,16,17,18,19)
# colnames(new_test_data)
# for(i in 1:ncol(new_test_data[,test_cols])) {
#   new_test_data[,test_cols][,i] <- as.factor(mapvalues
#                                           (new_test_data[,test_cols][,i], from =c("No internet service"),to=c("No")))
#  }


droplevels(new_train_data)->new_train_data
droplevels(new_test_data)->new_test_data


levels(new_test_data$Education)<-levels(new_train_data$Education)

train_impu_data<-centralImputation(new_train_data)
test_impu_data<-centralImputation(new_test_data)

str(train_impu_data)
str(test_impu_data)

sum(is.na(train_impu_data))
sum(is.na(test_impu_data))




str(test_impu_data)
#Feature generation
# impu_new_train_data$extra_fare=impu_new_train_data$TotalCharges - impu_new_train_data$BaseCharges
# impu_new_test_data$extra_fare=impu_new_test_data$TotalCharges - impu_new_test_data$BaseCharges


fare_churn<-sqldf("select churn,extra_fare from impu_new_train_data")
head(fare_churn)
#70% of customers who are using more than base plan are not churning
#22% of customers who are using less than base plan are churning
ggplot(fare_churn,aes(x=extra_fare,y=Churn))+geom_bar(stat = "identity")
plot(impu_new_train_data$extra_fare,impu_new_train_data$Churn)


#Churn based on contract type
contract_churn<-sqldf("select count(churn) as churn_count,ContractType from impu_new_train_data where churn='Yes' group by ContractType")
#1148/(1148+131+45)
contract_churn
ggplot(contract_churn,aes(x=ContractType,y=churn_count))+geom_bar(stat = "identity",fill="black")
#86% of customers churning are of month-to-month contract type


#Churn based on type of service
servicetype_churn<-sqldf("select count(churn) as churn_count ,TypeOfService from new_train_data where churn='Yes' group by TypeOfService")
ggplot(servicetype_churn,aes(x=TypeOfService,y=churn_count))+geom_bar(stat = "identity")


#Churn based on partner
partner_churn<-sqldf("select count(churn) as churn_count,HasPartner from impu_new_train_data  where churn='Yes' group by HasPartner")
ggplot(partner_churn,aes(x=HasPartner,y=churn_count))+geom_bar(stat = "identity")
#Customers who dont have partners are churning more

#Churn based on dependants
dependant_churn<-sqldf("select count(churn) as churn_count,HasDependents from impu_new_train_data where churn='Yes' group by HasDependents")
ggplot(dependant_churn,aes(x=HasDependents,y=churn_count))+geom_bar(stat = "identity")
dependant_churn
#81.8% of customers churning having no dependants



#Churn based on education level
education_churn<-sqldf("select count(churn) as churn_count,Education from train_impu_data where churn='Yes' group by Education")
education_churn
ggplot(education_churn,aes(x=Education,y=churn_count))+geom_bar(stat = "identity")
#(357+553)/(357+553+184+27+203)
#Highschool and under-graduate students are churning more
#68.7% churning by high school and ug qualfication 


#Churn based on electronic billing
elec_billing_churn<-sqldf("select count(churn) as Churn_count,ElectronicBilling from impu_new_train_data where churn='Yes' group by ElectronicBilling")
elec_billing_churn
ggplot(elec_billing_churn,aes(x=ElectronicBilling,y=Churn_count))+geom_bar(stat = "identity")
#75% of  customers churning on electronic billing 



#Train Val split
model_train_rows<-sample(nrow(train_impu_data),0.7*nrow(train_impu_data))
model_train_data<-train_impu_data[model_train_rows,]
model_val_data<-train_impu_data[-model_train_rows,]


str(train_impu_data)
str(test_impu_data)


#Model buliding
#Logistic regression

colnames(model_val_data)
glm_model<-glm(Churn~.,data=model_train_data[,-16],family = "binomial")
val_glm_pre<-predict(glm_model,model_val_data[,-16],type = "response")
val_glm_pre<-ifelse(val_glm_pre>0.30,"Yes","No")
table(val_glm_pre)
confusionMatrix(val_glm_pre,model_val_data$Churn,positive = "Yes")

colnames(test_impu_data)

test_pre<-predict(glm_model,test_impu_data[,-7],type = "response")
test_glm_pre<-ifelse(test_pre>0.3,"Yes","No")
table(test_glm_pre)


pre_sub<-cbind(target_id,test_glm_pre)
colnames(pre_sub)<-c("CustomerID","Churn")
write.csv(pre_sub,"predictions.csv",row.names = F)


head(target_id)


#Naive Bayes
naive_model<-naiveBayes(Churn~.,data=model_train_data[,-16])
val_naive_pre<-predict(naive_model,model_val_data[,-16])
val_glm_pre<-ifelse(val_glm_pre>0.30,"Yes","No")
table(val_glm_pre)


