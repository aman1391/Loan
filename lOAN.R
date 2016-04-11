train<- read.csv("train.csv" , stringsAsFactors = F)
test<- read.csv("test.csv" , stringsAsFactors = F)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(dummies)
str(test)
str(train)
test$Loan_Status<- "N"
combi<- rbind(train , test)

#little bit of cleaning required
combi$Gender<- as.factor(combi$Gender)
combi$Married<- as.factor(combi$Married)
combi$Dependents<- as.factor(combi$Dependents)
combi$Education<- as.factor(combi$Education)
combi$Self_Employed<- as.factor(combi$Self_Employed)
combi$Property_Area<- as.factor(combi$Property_Area)
combi$Loan_Status<- as.factor(combi$Loan_Status)
ggplot(combi , aes(Loan_Status)) + geom_bar(aes(fill=Married)) + facet_wrap(~Education)
ggplot(combi , aes(Loan_Status)) + geom_bar(aes(fill=Property_Area)) + facet_wrap(~Loan_Amount_Term)
ggplot(combi , aes(Loan_Status)) + geom_bar(aes(fill=Dependents)) + facet_wrap(~Education)
ggplot(combi , aes(Loan_Status)) + geom_bar(aes(fill=Self_Employed)) + facet_wrap(~Education)

levels(combi$Dependents)[levels(combi$Dependents)== ""]<- "0"
levels(combi$Gender)[levels(combi$Gender)== ""]<- "Male"
levels(combi$Married)[levels(combi$Married)== ""]<- "Yes"
levels(combi$Self_Employed)[levels(combi$Self_Employed)== ""]<- "No"
levels(combi$Dependents)[levels(combi$Dependents)== "3+"]<- "3"
levels(combi$Education)
levels(combi$Education)[levels(combi$Education)== "Not Graduate"]<- "0"
levels(combi$Education)[levels(combi$Education)== "Graduate"]<- "1"

#Feature engineeering
#Replacing na with -1
combi[is.na(combi)]<- -1
combi$LoanAmount[combi$LoanAmount== -1]<- mean(combi$LoanAmount)
combi$Credit_History[combi$Credit_History== -1]<- 1
combi$Loan_Amount_Term[combi$Loan_Amount_Term== -1]<- median(combi$Loan_Amount_Term)


combi$Loan_Status<- as.factor(combi$Loan_Status)
combi$ls<- with(combi , combi$ApplicantIncome+ combi$CoapplicantIncome)
hist(log(combi$ls) , breaks = 40)
combi$ls<- with(combi , log(combi$ls))
combi$loan<- with(combi, combi$LoanAmount/combi$ls)
combi$ds<- with(combi, (1.1*combi$LoanAmount)/combi$Loan_Amount_Term)

combi$ds<- with(combi , combi$Loan_Amount_Term*combi$ls)

combi<- combi[,-c(1,2,4,5,6,7,8,9,10,12)]
combi$ls<- log(combi$ls)
#Spilting the dataset
train1<- combi[1:nrow(train), ]
test1<- combi[-(1:nrow(train)), ]

#Random Forest
control <- trainControl(method = "cv", number = 5)
f_model <- train(Loan_Status ~. , data = train1, method = "rf", trControl = control, prox = TRUE, allowParallel = TRUE)
print(f_model) 
plot(f_model) 
forest_model <- randomForest(Loan_Status ~ . , data = train1, mtry = 5 , ntree = 100)
print(forest_model)
plot(forest_model)
main_predict <- predict(forest_model, newdata = test1, type = "response")
sub_file <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = main_predict)
write.csv(sub_file, 'r_f2.csv')


