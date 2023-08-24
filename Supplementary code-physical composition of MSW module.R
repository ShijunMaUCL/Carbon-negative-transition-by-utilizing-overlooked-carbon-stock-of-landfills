############################################################################physical composition of MSW module############################################################################
mydata=read.csv('PCMSWdata.csv',sep=",",header=TRUE)
head(mydata)

y8=sqrt(mydata$y8)
y7=sqrt(mydata$y7)
y6=sqrt(mydata$y6)
y5=sqrt(mydata$y5)
y4=sqrt(mydata$y4)
y3=sqrt(mydata$y3)
y2=sqrt(mydata$y2)
y1=sqrt(mydata$y1)
y0=sqrt(mydata$y0)
x8=acos(y8)
x7=acos(y7/sin(x8))
x6=acos(y6/(sin(x7)*sin(x8)))
x5=acos(y5/(sin(x6)*sin(x7)*sin(x8)))
x4=acos(y4/(sin(x5)*sin(x6)*sin(x7)*sin(x8)))
x3=acos(y3/(sin(x4)*sin(x5)*sin(x6)*sin(x7)*sin(x8)))
x2=acos(y2/(sin(x3)*sin(x4)*sin(x5)*sin(x6)*sin(x7)*sin(x8)))
x1=acos(y1/(sin(x2)*sin(x3)*sin(x4)*sin(x5)*sin(x6)*sin(x7)*sin(x8)))
df<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8)
write.csv(df,"C:/Users/13094/Desktop/Nature Cities/file/transformed data.csv")

#Neural network regression and supervised learning for physical composition of MSW(PCMSW) data
mydata=read.csv('PCMSWdata.csv',sep=",",header=TRUE)
head(mydata)
library("neuralnet")
fg=read.csv('transformed data.csv',sep=",",header=TRUE)
df=mydata[c(10,11)]
fg<-data.frame(fg,df)
set.seed(1)
max_data<-apply(fg,2,max)
min_data<-apply(fg,2,min)
data_scaled<-scale(fg,center=min_data,scale=max_data-min_data)
df=mydata[c(7,8,9)]
data_scaled<-data.frame(data_scaled,df)
index=sample(1:nrow(fg),round(0.70*nrow(fg)))
train_data<-as.data.frame(data_scaled[index,])
test_data<-as.data.frame(data_scaled[-index,])

#impact factor database of PCMSW processing
mydata1=read.csv('Socio-economic database.csv',sep=",",header=TRUE)
head(mydata1)
cl=mydata1[c(10,11)]
set.seed(1)
# max_data<-apply(cl,2,max)#
# min_data<-apply(cl,2,min)#

data_scaled1<-scale(cl,center=min_data[c(10,11)],scale=max_data[c(10,11)]-min_data[c(10,11)])
df=mydata1[c(7,8,9)]
data_scaled1<-data.frame(data_scaled1,df)
write.csv(data_scaled1,"C:/Users/13094/Desktop/Nature Cities/file/data_scaled1.csv")

#Neural network fitting of y1
fg1=data_scaled[c(2,10,11,12,13,14)]
n=names(fg1)
f1=as.formula(paste("x1~",paste(n[!n%in%"x1"],collapse="+")))
net_data1=neuralnet(f1,data=train_data,hidden=3,rep=1,linear.output = TRUE,threshold=0.1, learningrate = 0.1,algorithm = "slr", err.fct = "sse", act.fct = "logistic",stepmax = 1e+07)
predict_net_test11<-compute(net_data1,test_data[,10:14])
predict_net_test_start11<-predict_net_test11$net.result*(max(fg$x1)-min(fg$x1))+min(fg$x1)
test_start<-as.data.frame((test_data$x1)*(max(fg$x1)-min(fg$x1))+min(fg$x1))
MSE.net_data1<-sum((test_start-predict_net_test_start11)^2)/nrow(test_start)
MSE.net_data1
fit1 <- lm(test_data$x1 ~ predict_net_test11$net.result)
summary(fit1)
predict_net_test1<-compute(net_data1,data_scaled1)
predict_net_test_start1<-predict_net_test1$net.result*(max(fg$x1)-min(fg$x1))+min(fg$x1)
x11=predict_net_test_start1
x11<-data.frame(x11)
write.csv(x11,"C:/Users/13094/Desktop/Nature Cities/file/x11.csv")
rm(list = ls())

#Neural network fitting of y2
fg2=data_scaled[c(3,10,11,12,13,14)]
n=names(fg2)
f2=as.formula(paste("x2~",paste(n[!n%in%"x2"],collapse="+")))
net_data2=neuralnet(f2,data=train_data,hidden=5,linear.output = FALSE,rep=1,threshold=0.01, learningrate = 0.1, algorithm = "slr", err.fct = "sse", act.fct = "tanh")
predict_net_test12<-compute(net_data2,test_data[,10:14])
predict_net_test_start12<-predict_net_test12$net.result*(max(fg$x2)-min(fg$x2))+min(fg$x2)
test_start<-as.data.frame((test_data$x2)*(max(fg$x2)-min(fg$x2))+min(fg$x2))
MSE.net_data2<-sum((test_data$x2-predict_net_test12$net.result)^2)/nrow(test_start)
MSE.net_data2
fit2 <- lm(test_data$x2 ~ predict_net_test12$net.result)
summary(fit2)
predict_net_test2<-compute(net_data2,data_scaled1)
predict_net_test_start2<-predict_net_test2$net.result*(max(fg$x2)-min(fg$x2))+min(fg$x2)
x12=predict_net_test_start2
x12<-data.frame(x12)
write.csv(x12,"C:/Users/13094/Desktop/Nature Cities/file/x12.csv")

#Neural network fitting of y3
fg3=data_scaled[c(4,10,11,12,13,14)]
n=names(fg3)
f3=as.formula(paste("x3~",paste(n[!n%in%"x3"],collapse="+")))
net_data3=neuralnet(f3,data=train_data,hidden=2,rep=1,linear.output = TRUE,threshold=0.01, learningrate = 0.1, algorithm = "rprop+", err.fct = "sse", act.fct = "tanh")
predict_net_test13<-compute(net_data3,test_data[,10:14])
predict_net_test_start13<-predict_net_test13$net.result*(max(fg$x3)-min(fg$x3))+min(fg$x3)
test_start<-as.data.frame((test_data$x3)*(max(fg$x3)-min(fg$x3))+min(fg$x3))
MSE.net_data3<-sum((test_start-predict_net_test_start13)^2)/nrow(test_start)
MSE.net_data3
fit3 <- lm(test_data$x3 ~ predict_net_test13$net.result)
summary(fit3)
predict_net_test3<-compute(net_data3,data_scaled1)
predict_net_test_start3<-predict_net_test3$net.result*(max(fg$x3)-min(fg$x3))+min(fg$x3)
x13=predict_net_test_start3
x13<-data.frame(x13)
write.csv(x13,"C:/Users/13094/Desktop/Nature Cities/file/x13.csv")

#Neural network fitting of y4
fg4=data_scaled[c(5,10,11,12,13,14)]
n=names(fg4)
f4=as.formula(paste("x4~",paste(n[!n%in%"x4"],collapse="+")))
net_data4=neuralnet(f4,data=train_data,hidden=5,rep=1,linear.output = TRUE, threshold=0.01, learningrate = 0.1, algorithm = "slr", err.fct = "sse", act.fct = "tanh")
predict_net_test14<-compute(net_data4,test_data[,10:14])
predict_net_test_start14<-predict_net_test14$net.result*(max(fg$x4)-min(fg$x4))+min(fg$x4)
test_start<-as.data.frame((test_data$x4)*(max(fg$x4)-min(fg$x4))+min(fg$x4))
MSE.net_data4<-sum((test_start-predict_net_test_start14)^2)/nrow(test_start)
MSE.net_data4
fit4 <- lm(test_data$x4 ~ predict_net_test14$net.result)
summary(fit4)
predict_net_test4<-compute(net_data4,data_scaled1)
predict_net_test_start4<-predict_net_test4$net.result*(max(fg$x4)-min(fg$x4))+min(fg$x4)
x14=predict_net_test_start4
x14<-data.frame(x14)
write.csv(x14,"C:/Users/13094/Desktop/Nature Cities/file/x14.csv")

#Neural network fitting of y5
fg5=data_scaled[c(6,10,11,12,13,14)]
n=names(fg5)
f5=as.formula(paste("x5~",paste(n[!n%in%"x5"],collapse="+")))
net_data5=neuralnet(f5,data=train_data,hidden=5, rep=1,linear.output = FALSE,threshold=0.01, learningrate = 0.1, algorithm = "rprop+", err.fct = "sse", act.fct = "logistic")
predict_net_test15<-compute(net_data5,test_data[,10:14])
predict_net_test_start15<-predict_net_test15$net.result*(max(fg$x5)-min(fg$x5))+min(fg$x5)
test_start<-as.data.frame((test_data$x5)*(max(fg$x5)-min(fg$x5))+min(fg$x5))
MSE.net_data5<-sum((test_start-predict_net_test_start15)^2)/nrow(test_start)
MSE.net_data5
fit5 <- lm(test_data$x5 ~ predict_net_test15$net.result)
summary(fit5)
predict_net_test5<-compute(net_data5,data_scaled1)
predict_net_test_start5<-predict_net_test5$net.result*(max(fg$x5)-min(fg$x5))+min(fg$x5)
x15=predict_net_test_start5
x15<-data.frame(x15)
write.csv(x15,"C:/Users/13094/Desktop/Nature Cities/file/x15.csv")

#Neural network fitting of y6
fg6=data_scaled[c(7,10,11,12,13,14)]
n=names(fg6)
f6=as.formula(paste("x6~",paste(n[!n%in%"x6"],collapse="+")))
net_data6=neuralnet(f6,data=train_data,hidden=4,rep=1,linear.output = FALSE,threshold=0.01, learningrate = 0.1, algorithm = "slr", err.fct = "sse", act.fct = "logistic")
predict_net_test16<-compute(net_data6,test_data[,10:14])
predict_net_test_start16<-predict_net_test16$net.result*(max(fg$x6)-min(fg$x6))+min(fg$x6)
test_start<-as.data.frame((test_data$x6)*(max(fg$x6)-min(fg$x6))+min(fg$x6))
MSE.net_data6<-sum((test_start-predict_net_test_start16)^2)/nrow(test_start)
MSE.net_data6
fit6 <- lm(test_data$x6 ~ predict_net_test16$net.result)
summary(fit6)
predict_net_test6<-compute(net_data6,data_scaled1)
predict_net_test_start6<-predict_net_test6$net.result*(max(fg$x6)-min(fg$x6))+min(fg$x6)
x16=predict_net_test_start6
x16<-data.frame(x16)
write.csv(x16,"C:/Users/13094/Desktop/Nature Cities/file据/x16.csv")

#Neural network fitting of y8
fg7=data_scaled[c(8,10,11,12,13,14)]
n=names(fg7)
f7=as.formula(paste("x7~",paste(n[!n%in%"x7"],collapse="+")))
net_data7=neuralnet(f7,data=train_data,hidden=4,rep=1,linear.output = FALSE,threshold=0.01, learningrate = 0.1, algorithm = "rprop+", err.fct = "sse", act.fct = "tanh")
predict_net_test17<-compute(net_data7,test_data[,10:14])
predict_net_test_start17<-predict_net_test17$net.result*(max(fg$x7)-min(fg$x7))+min(fg$x7)
test_start<-as.data.frame((test_data$x7)*(max(fg$x7)-min(fg$x7))+min(fg$x7))
MSE.net_data7<-sum((test_start-predict_net_test_start17)^2)/nrow(test_start)
MSE.net_data7
fit7 <- lm(test_data$x7 ~ predict_net_test17$net.result)
summary(fit7)
predict_net_test7<-compute(net_data7,data_scaled1)
predict_net_test_start7<-predict_net_test7$net.result*(max(fg$x7)-min(fg$x7))+min(fg$x7)
x17=predict_net_test_start7
x17<-data.frame(x17)
write.csv(x17,"C:/Users/13094/Desktop/Nature Cities/file/x17.csv")

#Neural network fitting of y8
fg8=data_scaled[c(9,10,11,12,13,14)]
n=names(fg8)
f8=as.formula(paste("x8~",paste(n[!n%in%"x8"],collapse="+")))
net_data8=neuralnet(f8,data=train_data,hidden=4,rep=1,linear.output = TRUE, threshold=0.1, learningrate = 0.1, algorithm = "rprop-", err.fct = "sse", act.fct = "tanh")

predict_net_test18<-compute(net_data8,test_data[,10:14])
predict_net_test_start18<-predict_net_test18$net.result*(max(fg$x8)-min(fg$x8))+min(fg$x8)
test_start<-as.data.frame((test_data$x8)*(max(fg$x8)-min(fg$x8))+min(fg$x8))
MSE.net_data8<-sum((test_start-predict_net_test_start18)^2)/nrow(test_start)
MSE.net_data8
fit8 <- lm(test_data$x8 ~ predict_net_test18$net.result)
summary(fit8)
predict_net_test8<-compute(net_data8,data_scaled1)
predict_net_test_start8<-predict_net_test8$net.result*(max(fg$x8)-min(fg$x8))+min(fg$x8)
x18=predict_net_test_start8
x18<-data.frame(x18)
write.csv(x18,"C:/Users/13094/Desktop/Nature Cities/file/x18.csv")

#Neural networks predict data
c1=read.csv('x11.csv',sep=",",header=TRUE)
c2=read.csv('x12.csv',sep=",",header=TRUE)
c3=read.csv('x13.csv',sep=",",header=TRUE)
c4=read.csv('x14.csv',sep=",",header=TRUE)
c5=read.csv('x15.csv',sep=",",header=TRUE)
c6=read.csv('x16.csv',sep=",",header=TRUE)
c7=read.csv('x17.csv',sep=",",header=TRUE)
c8=read.csv('x18.csv',sep=",",header=TRUE)
x11=c1[,2]
x12=c2[,2]
x13=c3[,2]
x14=c4[,2]
x15=c5[,2]
x16=c6[,2]
x17=c7[,2]
x18=c8[,2]

y0=sin(x11)*sin(x12)*sin(x13)*sin(x14)*sin(x15)*sin(x16)*sin(x17)*sin(x18)
y1=cos(x11)*sin(x12)*sin(x13)*sin(x14)*sin(x15)*sin(x16)*sin(x17)*sin(x18)
y2=cos(x12)*sin(x13)*sin(x14)*sin(x15)*sin(x16)*sin(x17)*sin(x18)
y3=cos(x13)*sin(x14)*sin(x15)*sin(x16)*sin(x17)*sin(x18)
y4=cos(x14)*sin(x15)*sin(x16)*sin(x17)*sin(x18)
y5=cos(x15)*sin(x16)*sin(x17)*sin(x18)
y6=cos(x16)*sin(x17)*sin(x18)
y7=cos(x17)*sin(x18)
y8=cos(x18)
y0=y0^2
y1=y1^2
y2=y2^2
y3=y3^2
y4=y4^2
y5=y5^2
y6=y6^2
y7=y7^2
y8=y8^2
df3<-data.frame(y0,y1,y2,y3,y4,y5,y6,y7,y8)
write.csv(df3,"C:/Users/13094/Desktop/Nature Cities/file/GeneratedPCMSW.csv")