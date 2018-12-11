#Importing required libraries

library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(caret)
library(RANN)

# Reading the data


trdm <- read.csv("C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/traindemographics.csv")
trpf <- read.csv("C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/trainperf.csv")
trpv <- read.csv("C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/trainprevloans.csv")


ttdm <- read.csv("C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/testdemographics.csv")
ttpf <- read.csv("C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/testperf.csv")
ttpv <- read.csv("C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/testprevloans.csv")


# countFunc <- function(x){
#   sum(!is.na(x[mess_vars]) & x[mess_vars]!="None")
# }
# 
# 
# numconv <- function(x){
#   y <- as.numeric(levels(factor(x)))[x]
#   as.numeric()
# }
# 
# missFunc <- function(x){
#   gsub("Don't |999|-999|Ref", NA,x)
# }
# 


# Merging the datasets

trainloandf <- join_all(list(trdm,trpf,trpv), by = 'customerid', type = 'full')

testloandf <- join_all(list(ttdm,ttpf,ttpf), by = 'customerid', type = 'full')


# dropping nas
# trainloandf2 <- trainloandf1 %>% drop_na(bank_account_type,good_bad_flag)
# 
# testloandf2 <- testloandf1 %>% drop_na(bank_account_type)
trainloandf2a <- trainloandf %>% drop_na(systemloanid)
testloandf2 <- testloandf %>% drop_na(systemloanid)


colSums(is.na(trainloandf2a)) 


colSums(is.na(testloandf2))


# selecting vars for analysis
trainloandf1 <- trainloandf2a %>% select(-c(customerid,birthdate,longitude_gps,latitude_gps,systemloanid,referredby,bank_branch_clients,closeddate,firstduedate,firstrepaiddate,approveddate,creationdate))

testloandf1 <- testloandf2 %>% select(-c(birthdate,longitude_gps,latitude_gps,systemloanid,referredby,bank_branch_clients,approveddate,creationdate))

testloandf1a <- testloandf1
testloandf2a <- testloandf1a [-1]


colSums(is.na(trainloandf1))
colSums(is.na(testloandf2a))

# Impute missing values


library(mice)

#train

init1 <- mice(trainloandf1,maxit = 0)
meth1 <- init1$method
predMM <- init1$predictorMatrix

meth1[c("loannumber","loanamount","totaldue","termdays")] = ""

meth1[c("bank_account_type","bank_name_clients","employment_status_clients","level_of_education_clients")] = "polyreg"
meth1[c("level_of_education_clients")] = "polr"


set.seed(33)

imputed1 <- mice(trainloandf1,method = meth1,predictorMatrix = predMM,m=5)

trainloandf_com <- complete(imputed1)

anyNA(trainloandf_com)
colSums(is.na(trainloandf_com))

# test

init = mice(testloandf2a,maxit = 0)
meth = init$method
predM = init$predictorMatrix

meth[c("loannumber","loanamount","totaldue","termdays")] = ""

meth[c("bank_account_type","bank_name_clients","employment_status_clients","level_of_education_clients")] = "polyreg"
meth[c("level_of_education_clients")] = "polr"

set.seed(21)

imputed =mice(testloandf2a,method = meth,predictorMatrix = predM,m=4)

testloandf_com <- complete(imputed)



anyNA(testloandf_com)
colSums(is.na(testloandf_com))

#Convert to as_factor
trainloandf_com <- as_factor(trainloandf_com)
testloandf_com <- as_factor(testloandf_com)

#convert to dataframes
trainloandf_com <- data.frame(trainloandf_com)
testloandf_com <- data.frame(testloandf_com)

# Previewing the top data points
head(trainloandf_com)
head(testloandf_com)


str(trainloandf_com)
str(testloandf_com)


# Preparation of dates
# train

# cols_date <- c("birthdate","approveddate","creationdate","closeddate","firstduedate","firstrepaiddate")
# # trainloandf1[cols_date] <- sapply(trainloandf1[cols_date],as.Date)
# 
# trainloandf2 <- trainloandf1 %>% mutate_at(vars(birthdate,approveddate,creationdate,closeddate,firstduedate,firstrepaiddate), as.Date)
# 
# trainloandf2$birthyear <- lubridate::year(trainloandf2$birthdate)
# trainloandf2$creationyear <- lubridate::year(trainloandf2$creationdate)
# 
# trainloandf2$age <- as.numeric(trainloandf2$creationyear - trainloandf2$birthyear)
# 
# # test 
# testloandf1$birthdate <- as_date(testloandf1$birthdate)
# # testloandf1$approveddate <- as_date(testloandf1$approveddate)
# # testloandf1$creationdate <- as_date(testloandf1$creationdate)


# cols_date1 <- c("birthdate","approveddate","creationdate")
# testloandf1[cols_date1] <- sapply(testloandf1[cols_date1], as.Date)

# testloandf2 <- testloandf1 %>% mutate_at(vars(birthdate,approveddate,creationdate), as.Date) 



# create train and test

trainRowNumbers <- createDataPartition(trainloandf_com$good_bad_flag,p=0.7,list = F)

loandf_train <- trainloandf_com[trainRowNumbers,]
loandf_test <- trainloandf_com[-trainRowNumbers,]

# # Storing for later
x <- loandf_train[,-11]
y <- loandf_train$good_bad_flag
z <- loandf_test$good_bad_flag
z <- data.frame(z)
library(onehot)

encoder <- onehot(z)
z1 <- predict(encoder,z)

# One-Hot Encoding
# Creating dummy variables is converting a categorical variable to as many binary variables as here are categories.

dummies_model <- dummyVars(good_bad_flag ~.,data = loandf_train)

# Create the dummy variables using predict. The Y variable (good_bad_flag) will not be present in tr_m4.
loandf_train <- predict(dummies_model,newdata = loandf_train)

anyNA(loandf_train)

loandf_train <- data.frame(loandf_train)
                 

# Controlling for the range to between 0 and 1
pre_process_range <- preProcess(loandf_train,method = 'range')
loandf_train1 <- predict(pre_process_range,newdata = loandf_train)
str(loandf_train1)

# Append the Y variable
loandf_train1$good_bad_flag <- y

apply(loandf_train1[,1:10],2,FUN = function(x){c('min'=min(x),'max' = max(x))})

# mars
set.seed(123)
model_marrs = train(good_bad_flag ~., data = loandf_train1, method = 'earth')
fitted <- predict(model_marrs)

model_marrs

plot(model_marrs, main = 'Model Mars Accuracies')

var_imp_Mars <- varImp(model_marrs)
plot(var_imp_Mars, main = 'Variable Importance with Mars')

# random forest
set.seed(111)
rf = train(good_bad_flag~.,data = loandf_train1,method = 'rf')

rf

# svm
set.seed(131)
model_svm = train(good_bad_flag~., data = loandf_train1, method = 'svmRadial')
model_svm


# Preparing test data

#pretest
loandf_test$good_bad_flag <- NA
loandf_test1 <- predict(dummies_model,newdata = loandf_test)
pre_process_range2 <- preProcess(loandf_test1, method = 'range')
loandf_test2 <- predict(pre_process_range2, loandf_test1)

loandf_test2 <- data.frame(loandf_test2)

predicts <- predict(model_marrs, loandf_test2)
predicts_rf <- predict(rf, loandf_test2)
predicts_svm <- predict(model_svm, loandf_test2)

predicts <- data.frame(predicts)
head(predicts)

predicts_rf <- data.frame(predicts_rf)
head(predicts_rf)

predicts_svm <- data.frame(predicts_svm)
head(predicts_svm)

encodder <- onehot(predicts)
predicts1 <- predict(encodder,predicts)

caret::MAE(predicts1,z1)

encodder1 <- onehot(predicts_rf)
predicts_rf1 <- predict(encodder1,predicts_rf)

caret::MAE(predicts_rf1,z1)

encodder2 <- onehot(predicts_svm)
predicts_svm1 <- predict(encodder2,predicts_svm)

caret::MAE(predicts_svm1,z1)


# create variable of prediction

testloandf_com$good_bad_flag <- NA
testloandf_com <- as_factor(testloandf_com)


dummies_model1 <- dummyVars(good_bad_flag ~.,data = testloandf_com)
testloandf_com1 <- predict(dummies_model1, newdata = testloandf_com)

pre_process_range1 <- preProcess(testloandf_com1, method = 'range')
testloandf_com1 <- predict(pre_process_range1, newdata = testloandf_com1)
testloandf_com1 <- data.frame(testloandf_com1) 

str(testloandf_com1)
# Prediction earth
predicted <- predict(model_marrs,testloandf_com1)
predicted_rf <- predict(rf,testloandf_com1)


predicted <- data.frame(predicted)
predicted_rf <- data.frame(predicted_rf)
head(predicted)
head(predicted_rf)

predicted <- predicted %>% mutate(predicted = recode(predicted,"Bad" = "0","Good" = "1")) %>% mutate(predicted = as.numeric(predicted))
predicted_rf <- predicted_rf %>% mutate(predicted_rf = recode(predicted_rf,"Bad" = "0","Good" = "1")) %>% mutate(predicted_rf = as.numeric(predicted_rf))




colnames(predicted)[1] <- "Good_Bad_flag"
colnames(predicted_rf)[1] <- "Good_Bad_flag"


predicted_t1 <- cbind(predicted,testloandf1a)
predicted_t2 <- cbind(predicted_rf,testloandf1a)


submission1 <- predicted_t1 %>% select(customerid,Good_Bad_flag)
submission2 <- predicted_t2 %>% select(customerid,Good_Bad_flag)


# x <- names(loandf_train1)
# y <- names(testloandf3)
# 
# z <- setdiff(x,y)
# z1 <- setdiff(y,x)


# x1 <- predicted_ts
# y1 <- testloandf2a

# z2 <- setdiff(x1,y1)
# printing output
write.csv(z2,"C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/diff.csv",row.names = F)
write.csv(predicted_ts,"C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/x.csv",row.names = F)
write.csv(testloandf2a,"C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/y.csv",row.names = F)



write.csv(submission1,"C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/Submission1.csv",row.names = F)
write.csv(submission2,"C:/Users/dmunene/OneDrive - Dalberg Global Development Advisors/RESSOL/Personal/Data Analysis/Loan Default/Submission2.csv",row.names = F)



yxz <- unique(predicted_t2)
yxz <- data.frame(yxz)
