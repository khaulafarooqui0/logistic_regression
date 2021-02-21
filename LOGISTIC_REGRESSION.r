file.choose()
read.csv("C:\\Users\KhaulA\\R_Practise\\MODULE6-LOGISTIC_EXPLORATION\\Customer_Churn.csv",stringsAsFactors=T)->customer_churn
View(customer_churn)
str(customer_churn)
glm(Churn ~ TechSupport, customer_churn, family="binomial")-> model1
summary(model1)
predict(model1, data.frame(TechSupport="Yes"),type="response")
predict(model1, data.frame(TechSupport="No"),type="response")
predict(model1, data.frame(TechSupport="No internet service"),type="response")


glm(Dependents~tenure, customer_churn,family="binomial")->model2
#a.	Have a glance at the summary of the model built
summary(model2)

#b.	Predict the result when the value of 'tenure' is 10
predict(model2, data.frame(tenure=10),type="response")
#c.	Predict the result when the value of 'tenure' is 50
predict(model2,data.frame(tenure=50),type="response")
#d.	Predict the result when the value of 'tenure' is 70
predict(model2,data.frame(tenure=70),type="response")

#1.	Build a multiple logistic regression model:
#a.	Start off by dividing the data-set into 'train' & 'test' sets in 65:35 ratio, with the split-criteria being determined by 'gender' column

install.packages(caTools)
library(caTools)
sample.split(customer_churn$gender,SplitRatio = 0.65)-> div
View(div)
subset(customer_churn, div==T)->train
View(train)
subset(customer_churn, div==F)->test
View(test)
table(div)
#b.	Build a logistic regression model on the train set where the dependent variable is 'gender' & the independent variables are 'Dependents', 'InternetService' & 'Contract' & store the result in 'log_mod_multi'

glm(gender~ Dependents+InternetService+Contract,train,family = "binomial")->log_mod_multi
head(log_mod_multi)

#c.	Predict the values on top of the test set & store the result in 'result_log_multi'
predict(log_mod_multi,newdata=test,type="response")->result_log_multi
result_log_multi

#d.	Have a look at the range of 'result_log_multi' & build a confusion matrix where the threshold of predicted values is greater than '0.49'
result_log_multi
range(result_log_multi)

#syntax for confusion martix
#table(actual,predicted)
#method 1
table(test$gender, result_log_multi>0.49)->acc
acc
sum(diag(acc))/sum(acc)
#FALSE TRUE
#         TP   FP
#Female   109 1112
#Male     137 1107
#          FN   TN
#    Formula for confusion matrix(TP+TN)/(TP+TN+FP+FN)

#e.	Calculate the accuracy of the model from the confusion matrix


#method 2
ifelse(result_log_multi>0.49,"Female","Male")->lm
lm
table(test$gender,lm)->a

#e.	Calculate the accuracy of the model from the confusion matrix
 sum(diag(a))/sum(a)

#step1 check the range manualy and chekh the threshold value and generate confusion matrix
#step2 plot a graph and fpich the threshold value from graph and conf matrix#
#step3 use th erocr package  and directly geet the accuracy


#          Female Male
#Female   1112  109
#Male     1107  137  Herer females are  predicted  as females 1112 times and femalesa re predicted as males 109 times,
#                    males are m predicted as females 1107 times and males are predicted as males 137 times this is confusion matrix


 
 
 #2.	Build second logistic regression model on the same 'train' & 'test' sets
 #a.	 In this case dependent variable is 'gender' & the independent variables are 'tenure', 'MonthlyCharges' & 'PaymentMethod
sample.split(customer_churn$gender,SplitRatio=0.65)->div1
div1
subset(customer_churn,div=T)->train
subset(customer_churn,div=F)->test

glm(gender~tenure+MonthlyCharges+PaymentMethod,data=train,family="binomial")->log_mod2
summary(log_mod2)


predict(log_mod2,newdata=test,type="response")->result_log_multi2
result_log_multi2
#c.	Have a look at the range of 'result_log_multi2' & build a confusion matrix where the threshold of predicted values is greater than 0.49
range(result_log_multi2)



#actaul predicted
table(test$gender,result_log_multi2>0.49)->ab
ab
 sum(diag(ab))/sum(ab)->acc_per
acc_per 




#Questions on ROCR package:
#1.	Build a logistic regression model:
#a.	Start off by dividing the data-set into 'train' & 'test' sets in 80:20 ratio, with the split-criteria being determined by 'Churn' column

sample.split(customer_churn$Churn,SplitRatio=0.80)->div3
div3
subset(customer_churn,div2=T)->train3
subset(customer_churn,div2=F)->test3
#b.	Build a logistic regression model on the train set where the dependent variable is 'Churn' & the independent variables are 'MonthlyCharges', 'tenure' & 'TechSupport' & store the result in 'log_mod_roc'

glm(Churn~MonthlyCharges+tenure+TechSupport,data=train3,family="binomial")->log_mod_roc
summary(log_mod_roc)
#c.	Predict the values on top of the test set & store the result in 'result_log_roc'
predict(log_mod_roc,newdata=test3,type="response")->result_log_roc
result_log_roc


 range(result_log_roc)
#d.	Use the performance() function from the ROCR package & build the 'Accuracy vs cut-off' plot
 install.packages("ROCR")
 library("ROCR")
 prediction(result_log_roc,test3$Churn)->pred_log
 performance(pred_log,"acc")->acc
plot(acc) 
table(test3$Churn,result_log_roc>0.62)->conf_mat
conf_mat
sum(diag(conf_mat))/sum(conf_mat)
 
 #e.	Plot the 'ROC' curve
performance(pred_log,"tpr","fpr")->area_under_curve
plot(area_under_curve)
#f.	Find out the "area under the curve"
performance(pred_log,"auc")->auc
auc@y.values
 
