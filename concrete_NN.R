Data_concrete=read.csv(file.choose())
str(Data_concrete)
summary(Data_concrete)
plot(Data_concrete$strength,Data_concrete$cement)
plot(Data_concrete$strength,Data_concrete$slag)
plot(Data_concrete$strength,Data_concrete$ash)
plot(Data_concrete$strength,Data_concrete$water)
plot(Data_concrete$strength,Data_concrete$superplastic)
plot(Data_concrete$strength,Data_concrete$coarseagg)
plot(Data_concrete$strength,Data_concrete$fineagg)
plot(Data_concrete$strength,Data_concrete$age)
pairs(Data_concrete)
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
norm_data=as.data.frame(lapply(Data_concrete,normalize))
Train_data=norm_data[c(1:824),]
Test_data=norm_data[c(825:1030),]
library(neuralnet)
#Model on train data
model_1=neuralnet(Train_data$strength~.,data = Train_data)
pred_train_model1=as.data.frame(predict(model_1,newdata = Train_data))
cor(Train_data$strength,pred_train_model1)
model_2=neuralnet(Train_data$strength~.,data = Train_data,hidden = 2)
pred_train_model2=as.data.frame(predict(model_2,newdata = Train_data))
cor(Train_data$strength,pred_train_model2)
model_3=neuralnet(Train_data$strength~.,data = Train_data,hidden = 3)
pred_train_model3=as.data.frame(predict(model_3,newdata = Train_data))
cor(Train_data$strength,pred_train_model3)
model_4=neuralnet(Train_data$strength~.,data = Train_data,hidden = 5)
pred_train_model4=as.data.frame(predict(model_4,newdata = Train_data))
cor(Train_data$strength,pred_train_model4)
#Final model
Final_model=neuralnet(Train_data$strength~.,data = Train_data,hidden = 10)
summary(Final_model)
plot(Final_model)
pred_train_finalmodel=as.data.frame(predict(Final_model,newdata=Train_data))
cor(Train_data$strength,pred_train_finalmodel)
#Prediction on test data
pred_test_data=as.data.frame(predict(Final_model,newdata=Test_data))
cor(Test_data$strength,pred_test_data)
#Accuracy(Test data)=94.53%