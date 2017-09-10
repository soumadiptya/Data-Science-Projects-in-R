
#Project 2 - Question 1
#Breaking data into 2 random parts
set.seed(2)
s=sample(1:nrow(bd_final),0.7*nrow(bd_final))

bd_train=bd_final[s,]
bd_test=bd_final[-s,]

#Question 2- Removing predictor values with VIF>5

install.packages("car")
library(car)

for_vif=lm(y_value~.,data=bd_train)
vif(for_vif)
sort(vif(for_vif),decreasing = T)

#Removing month_new_7 from the data
for_vif=lm(y_value~.-month_new_7,data=bd_train)
sort(vif(for_vif),decreasing = T)

#Removing job_new_1 from the data
for_vif=lm(y_value~.-month_new_7-job_new_1,data=bd_train)
sort(vif(for_vif),decreasing = T)

#Removing education_2 from the data
for_vif=lm(y_value~.-month_new_7-job_new_1-education_2,data=bd_train)
sort(vif(for_vif),decreasing = T)

#All VIF values are now <5

#Question 3- Performing Logistic Regression on the data set
bd_fit=bd_train %>% select(-month_new_7,-job_new_1,-education_2)

fit_initial=glm(y_value~.,family="binomial",data=bd_fit)
summary(fit_initial)

#Excluding variables using the step function
fit_initial=step(fit_initial)

summary(fit_initial)

#There are some variables with p>0.05. Hence removing them sequentially
formula(fit_initial)
fit_1=glm(y_value ~ age + balance + day + duration + campaign + previous + 
            job_new_2 + job_new_6 + job_new_7 + education_1 + education_3 + 
            marital_2 + housing_1 + contact_1 + contact_2 + loan_1 + 
            month_new_1 + month_new_2 + month_new_3 + month_new_5 + month_new_6 + 
            month_new_8 + month_new_9 + poutcome_1 + poutcome_2 + poutcome_3,data=bd_train,
            family="binomial")

#Excluding "previous" based on p-value

fit_1=glm(y_value ~ age + balance + day + duration + campaign + 
            job_new_2 + job_new_6 + job_new_7 + education_1 + education_3 + 
            marital_2 + housing_1 + contact_1 + contact_2 + loan_1 + 
            month_new_1 + month_new_2 + month_new_3 + month_new_5 + month_new_6 + 
            month_new_8 + month_new_9 + poutcome_1 + poutcome_2 + poutcome_3,data=bd_train,
          family="binomial")

summary(fit_1)


#Excluding job_new_2

fit_1=glm(y_value ~ age + balance + day + duration + campaign + 
            job_new_6 + job_new_7 + education_1 + education_3 + 
            marital_2 + housing_1 + contact_1 + contact_2 + loan_1 + 
            month_new_1 + month_new_2 + month_new_3 + month_new_5 + month_new_6 + 
            month_new_8 + month_new_9 + poutcome_1 + poutcome_2 + poutcome_3,data=bd_train,
          family="binomial")

summary(fit_1)

#All values now have p<0.05
#Getting score on train data

bd_train$score=predict(fit_1,newdata=bd_train,type="response")

#Plotting the scores
library(ggplot2) 
ggplot(bd_train,aes(y=y_value,x=score,color=factor(y_value)))+ geom_point()+geom_jitter()

#Question #4- To calculate cut offs
#create data set with all Metrics defaulted to 0

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,TN=0,FN=0)
#Select cut off values from 0 to 1 for iterating over
cutoffs=seq(0,1,len=100)

#Design for loop to calculate Metrics for different values of cutoff

for(cutoff in cutoffs){
predicted=as.numeric(bd_train$score>cutoff)
TP=sum(predicted==1 & bd_train$y_value==1)
TN=sum(predicted==0 & bd_train$y_value==0)
FP=sum(predicted==1 & bd_train$y_value==0)
FN=sum(predicted==0 & bd_train$y_value==1)
cutoff_data=rbind(cutoff_data, c(cutoff,TP,FP,TN,FN))
}
#Removing the top row (since it's dummy data)
cutoff_data=cutoff_data[-1,]

#Calculating the Metrics and Ks

cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  select(-P,-N)

#Plotting KS cutoff vs Score for verification
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=KS))+geom_line()

#Selecting the cut off with maximum KS

KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]
KS_cutoff

#Predicting on test data to see Confusion Matrix
bd_test$score=predict(fit_1,newdata = bd_test,type = "response")
table(bd_test$y_value,as.numeric(bd_test$score>KS_cutoff))

Error_Logistic=(2404+152)/(8321+2404+152+1155)

#Question #5- Building a Random Forest Model
# We must exclude Score from the train and test data set as it is not a part of the original variables

glimpse(bd_train)
bd_train=bd_train %>% select(-score)
bd_test=bd_test %>% select(-score)

#Loading Random Forest model
install.packages("randomForest")
library(randomForest)


# Converting y_value to a factor type to perform classification
bd_train$y_value=as.factor(ifelse(bd_train$y_value==1,"Yes","No"))
bd_test$y_value=as.factor(ifelse(bd_test$y_value==1,"Yes","No"))

#Building a Random Forest on this

model_rf=randomForest(y_value~.,data=bd_train)

forest.pred=predict(model_rf,newdata = bd_test)
table(bd_test$y,forest.pred)

Error_forest=(816+237)/(10488+237+816+491)
Error_forest

#Question #6- Variable and Importance Plot for top 6 variables
importance(model_rf)
varImpPlot(model_rf)

#Hence top 6 variables are duration,balance,age,day,poutcome_3,pdays
#Building a dataset with these 6 variables only

bd_new_train=bd_train %>% select(duration,balance,age,day,poutcome_3,pdays,y_value)
bd_new_test=bd_test %>% select(duration,balance,age,day,poutcome_3,pdays,y_value)

glimpse(bd_new_train)

#Converting y_value to numeric type again
bd_new_train=bd_new_train %>% mutate(y_value=as.numeric(y_value))
#Checking for VIF
for_vif=lm(y_value~.,data=bd_new_train)
vif(for_vif)

#VIF's are close to 1 for all values so creating the Logistic regression Model
fit_new_train=glm(y_value~.,family="binomial",data=bd_new_train)
#Error in eval(expr, envir, enclos) : y values must be 0 <= y <= 1
bd_new_train=bd_new_train %>% mutate(y_value=ifelse(y_value==1,0,1))
                                     
summary(fit_new_train)

#All variables have p values <<0.05
#Getting score on train data

bd_new_train$score=predict(fit_new_train,newdata=bd_new_train,type="response")

#Plotting them
library(ggplot2) 
ggplot(bd_new_train,aes(y=y_value,x=score,color=factor(y_value)))+ geom_point()+geom_jitter()

cutoff_data_1=data.frame(cutoff=0,TP=0,FP=0,TN=0,FN=0)
#Select cut off values from 0 to 1 for iterating over
set.seed(2)
cutoffs=seq(0,1,len=100)

#Design for loop to calculate Metrics for different values of cutoff

for(cutoff in cutoffs){
  predicted=as.numeric(bd_new_train$score>cutoff)
  TP=sum(predicted==1 & bd_new_train$y_value==1)
  TN=sum(predicted==0 & bd_new_train$y_value==0)
  FP=sum(predicted==1 & bd_new_train$y_value==0)
  FN=sum(predicted==0 & bd_new_train$y_value==1)
  cutoff_data_1=rbind(cutoff_data_1, c(cutoff,TP,FP,TN,FN))
}
#Removing the top row (since it's dummy data)
cutoff_data_1=cutoff_data_1[-1,]

#Calculating the Metrics and Ks

cutoff_data_1=cutoff_data_1 %>%
  mutate(P=FN+TP,N=TN+FP) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  select(-P,-N)

#Plotting KS cutoff vs Score for verification
library(ggplot2)
ggplot(cutoff_data_1,aes(x=cutoff,y=KS))+geom_line()

#Selecting the cut off with maximum KS

KS_cutoff=cutoff_data_1$cutoff[which.max(cutoff_data_1$KS)][1]
KS_cutoff

#Predicting on test data to see Confusion Matrix
bd_new_test$score=predict(fit_new_train,newdata = bd_new_test,type = "response")
table(bd_new_test$y_value,as.numeric(bd_new_test$score>KS_cutoff))

Error_updated=(316+2074)/(8651+2074+316+991)
Error_updated
#Hence there is a slight improvement in the Accuracy from 21.24% to 19.86% when using the top 6 variables only
