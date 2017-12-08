rm(list=ls(all=TRUE))


#Reading the data
setwd("~/Internship/ml-prove") #Changing the WD
raw_data<-read.csv("raw_data.csv",header = F)
head(raw_data)
dim(raw_data)
nrow(raw_data)
head(raw_data$V58)

ik<-raw_data
table(raw_data$V54)

#Data Preprocessing
#Modifying target variable
#If the Heuristic is proved in 100 seconds it is marked as 1 else -100 is marked as 0
for (i in 1:nrow(ik)){
  if(ik$V54[i]==-100){
    ik$V54[i]=0}
  else if(ik$V54[i]!=-100){
    ik$V54[i]=1
    
}
}

for (i in 1:nrow(ik)){
  if(ik$V55[i]==-100){
    ik$V55[i]=0}
  else if(ik$V55[i]!=-100){
    ik$V55[i]=1
    
  }
}

for (i in 1:nrow(ik)){
  if(ik$V56[i]==-100){
    ik$V56[i]=0}
  else if(ik$V56[i]!=-100){
    ik$V56[i]=1
    
  }
}

for (i in 1:nrow(ik)){
  if(ik$V57[i]==-100){
    ik$V57[i]=0}
  else if(ik$V57[i]!=-100){
    ik$V57[i]=1
    
  }
}

for (i in 1:nrow(ik)){
  if(ik$V58[i]==-100){
    ik$V58[i]=0}
  else if(ik$V58[i]!=-100){
    ik$V58[i]=1
    
  }
}
table(ik$V54)
table(raw_data$V54)
table(ik$V55)
table(raw_data$V55)
table(ik$V56)
table(raw_data$V56)
table(ik$V57)
table(raw_data$V57)
table(ik$V58)
table(raw_data$V58)
ik$V59<-NA


#Feature Engineering
#New feature Heuristic-6 is introduced(If all the 5 heuristics failed
#-to prove in 100 seconds then 6th heuristic is 1 else 0)
for (j in 1:nrow(ik)){
  
if(ik$V54[j]==0 && ik$V55[j]==0 && ik$V56[j]==0 && ik$V57[j]==0 && ik$V58[j]==0){
  ik$V59[j]=1
}
else{
  ik$V59[j]=0
}
}

#Changing names of the columns

#Static Features col name creation

static_features <- sprintf("Sta_%s",seq(1:14))
static_features

#Dynamic Features col name creation
dynamic_features<-sprintf("Dyn_%s",seq(15:53))
dynamic_features

#Huerisic features col name creation
heuristic_tar<-sprintf("Heuristic_%s",seq(1:6))
heuristic_tar



a<-c(static_features,dynamic_features,heuristic_tar)

col_names<-as.matrix(c(static_features,dynamic_features,heuristic_tar),1,59,byrow=F)
col_names<-as.factor(col_names)
colnames(ik)<-col_names
head(ik)

library(corrplot)
corrplot(cor(new_data))

#Visualization of data
#Box-whisker plot for the data

# for(i in 1:53){
#   boxplot(ik[,i],xlab=names(ik[i]))
#   dev.copy(jpeg,filename=paste(names(ik[i]),"plot.jpg",sep ="-"))
#   
#   dev.off()
# }


#There are 2554 instances in which all heuristics failed to prove


#Two redundant features(Static 5 and static 21 have 0's in whole columns) 
colnames(ik)
new_data<-subset(ik,select = -c(5,35))
colnames(new_data)

#No missing values
sum(is.na(new_data))

#Checking for  class-Imbalance in 
dim(new_data)
colnames(new_data)

for (m in 52:57){
  print("Proportion of classes")
  print(prop.table(table(new_data[,m])))
  print(colnames(new_data[m]))
  
}



#The difference between majority class and minority class is very low.
#So  no need of under sampling or over sampling techniques

factor_cols<-c(52:57)
new_data[,factor_cols]<-lapply(new_data[,factor_cols],factor)
str(new_data)



colnames(new_data)
str(new_data)

library("vegan")
library("caret")

# target<-subset(new_data,select = c(52,53,54,55,56,57))
# preObj <- preProcess(new_data[, -c(52,53,54,55,56,57)], method=c("center", "scale"))
# 
# newData <- predict(preObj, new_data[, -c(52,53,54,55,56,57)])
# 
# std_data<-cbind(newData,target)


summary(newData)
train_rows<-sample(nrow(new_data),0.8*nrow(new_data))              
train_data<-new_data[train_rows,]
test_data<-new_data[-train_rows,]

#Logistic models
#Heuristic 1

colnames(train_data)




glm_h1<-glm(Heuristic_1~.,train_data[,-c(53,54,55,56,57)],family = "binomial")
glm_pre_h1<-predict(glm_h1,test_data[,-c(53,54,55,56,57)],type="response")
glm_pre_h1<-ifelse(glm_pre_h1>0.4,1,0)
tab_glm_h1<-table(glm_pre_h1,test_data$Heuristic_1)
con_glm_h1<-confusionMatrix(tab_glm_h1)
con_glm_h1


glm_h2<-glm(Heuristic_2~.,train_data[,-c(52,54,55,56,57)],family = "binomial")
glm_pre_h2<-predict(glm_h2,test_data[,-c(52,54,55,56,57)],type="response")
glm_pre_h2<-ifelse(glm_pre_h2>0.4,1,0)
tab_glm_h2<-table(glm_pre_h2,test_data$Heuristic_2)
con_glm_h2<-confusionMatrix(tab_glm_h2)
con_glm_h2


glm_h3<-glm(Heuristic_3~.,train_data[,-c(52,53,55,56,57)],family = "binomial")
glm_pre_h3<-predict(glm_h3,test_data[,-c(52,53,55,56,57)],type="response")
glm_pre_h3<-ifelse(glm_pre_h3>0.4,1,0)
tab_glm_h3<-table(glm_pre_h3,test_data$Heuristic_3)
con_glm_h3<-confusionMatrix(tab_glm_h3)
con_glm_h3


glm_h4<-glm(Heuristic_4~.,train_data[,-c(52,53,54,56,57)],family = "binomial")
glm_pre_h4<-predict(glm_h4,test_data[,-c(52,53,54,56,57)],type="response")
glm_pre_h4<-ifelse(glm_pre_h4>0.4,1,0)
tab_glm_h4<-table(glm_pre_h4,test_data$Heuristic_4)
con_glm_h4<-confusionMatrix(tab_glm_h4)
con_glm_h4


glm_h5<-glm(Heuristic_5~.,train_data[,-c(52,53,54,55,57)],family = "binomial")
glm_pre_h5<-predict(glm_h5,test_data[,-c(52,53,54,55,57)],type="response")
glm_pre_h5<-ifelse(glm_pre_h5>0.4,1,0)
tab_glm_h5<-table(glm_pre_h5,test_data$Heuristic_5)
con_glm_h5<-confusionMatrix(tab_glm_h5)
con_glm_h5



glm_h6<-glm(Heuristic_6~.,train_data[,-c(52,53,54,55,56)],family = "binomial")
glm_pre_h6<-predict(glm_h6,test_data[,-c(52,53,54,55,56)],type="response")
glm_pre_h6<-ifelse(glm_pre_h6>0.4,1,0)
tab_glm_h6<-table(glm_pre_h6,test_data$Heuristic_6)
con_glm_h6<-confusionMatrix(tab_glm_h6)
con_glm_h6




#Static features

sta_rows<-c(1:12,52:57)
sta_train_data<-train_data[,sta_rows]
sta_test_data<-test_data[,sta_rows]

str(sta_train_data)


std_sta_train_data<-preProcess(sta_train_data,method = c("center"))
sta_train_data<-predict(std_sta_train_data,sta_train_data)

std_sta_test_data<-preProcess(sta_test_data,method = c("center"))
sta_test_data<-predict(std_sta_test_data,sta_test_data)

colnames(sta_train_data)

sta_glm_h1<-glm(Heuristic_1~.,sta_train_data[,-c(14,15,16,17,18)],family = "binomial")
sta_glm_pre_h1<-predict(sta_glm_h1,sta_test_data[,-c(14,15,16,17,18)],type="response")
sta_glm_pre_h1<-ifelse(sta_glm_pre_h1>0.4,1,0)
sta_tab_glm_h1<-table(sta_glm_pre_h1,sta_test_data$Heuristic_1)
sta_con_glm_h1<-confusionMatrix(sta_tab_glm_h1)
sta_con_glm_h1



sta_glm_h2<-glm(Heuristic_2~.,sta_train_data[,-c(13,15,16,17,18)],family = "binomial")
sta_glm_pre_h2<-predict(sta_glm_h2,sta_test_data[,-c(13,15,16,17,18)],type="response")
sta_glm_pre_h2<-ifelse(sta_glm_pre_h2>0.4,1,0)
sta_tab_glm_h2<-table(sta_glm_pre_h2,sta_test_data$Heuristic_2)
sta_con_glm_h2<-confusionMatrix(sta_tab_glm_h2)
sta_con_glm_h2

colnames(sta_train_data)
sta_glm_h3<-glm(Heuristic_3~.,sta_train_data[,-c(13,14,16,17,18)],family = "binomial")
sta_glm_pre_h3<-predict(sta_glm_h3,sta_test_data[,-c(13,14,16,17,18)],type="response")
sta_glm_pre_h3<-ifelse(sta_glm_pre_h3>0.95,0,1)
sta_tab_glm_h3<-table(sta_glm_pre_h3,sta_test_data$Heuristic_3)
sta_con_glm_h3<-confusionMatrix(sta_tab_glm_h3)
sta_con_glm_h3



sta_glm_h4<-glm(Heuristic_4~.,sta_train_data[,-c(13,14,15,17,18)],family = "binomial")
sta_glm_pre_h4<-predict(sta_glm_h4,sta_test_data[,-c(13,14,15,17,18)],type="response")
sta_glm_pre_h4<-ifelse(sta_glm_pre_h4>0.95,0,1)
sta_tab_glm_h4<-table(sta_glm_pre_h4,sta_test_data$Heuristic_4)
sta_con_glm_h4<-confusionMatrix(sta_tab_glm_h4)
sta_con_glm_h4




sta_glm_h5<-glm(Heuristic_5~.,sta_train_data[,-c(13,14,15,16,18)],family = "binomial")
sta_glm_pre_h5<-predict(sta_glm_h5,sta_test_data[,-c(13,14,15,16,18)],type="response")
sta_glm_pre_h5<-ifelse(sta_glm_pre_h5>0.95,0,1)
sta_tab_glm_h5<-table(sta_glm_pre_h5,sta_test_data$Heuristic_5)
sta_con_glm_h5<-confusionMatrix(sta_tab_glm_h5)
sta_con_glm_h5



sta_glm_h6<-glm(Heuristic_6~.,sta_train_data[,-c(13,14,15,16,17)],family = "binomial")
sta_glm_pre_h6<-predict(sta_glm_h6,sta_test_data[,-c(13,14,15,16,17)],type="response")
sta_glm_pre_h6<-ifelse(sta_glm_pre_h6>0.05,0,1)
sta_tab_glm_h6<-table(sta_glm_pre_h6,sta_test_data$Heuristic_6)
sta_con_glm_h6<-confusionMatrix(sta_tab_glm_h6)
sta_con_glm_h6


#Dynamic features
colnames(dyn_train_data)

dyn_rows<-c(14:51,52:57)
dyn_train_data<-train_data[,dyn_rows]
dyn_test_data<-test_data[,dyn_rows]

colnames(dyn_train_data)
dyn_glm_h1<-glm(Heuristic_1~.,dyn_train_data[,-c(40,41,42,43,44)],family = "binomial")
dyn_glm_pre_h1<-predict(dyn_glm_h1,dyn_test_data[,-c(40,41,42,43,44)],type="response")
dyn_glm_pre_h1<-ifelse(dyn_glm_pre_h1>0.4,1,0)
dyn_tab_glm_h1<-table(dyn_glm_pre_h1,sta_test_data$Heuristic_1)
dyn_con_glm_h1<-confusionMatrix(dyn_tab_glm_h1)
dyn_con_glm_h1


dyn_glm_h2<-glm(Heuristic_2~.,dyn_train_data[,-c(39,41,42,43,44)],family = "binomial")
dyn_glm_pre_h2<-predict(dyn_glm_h2,dyn_test_data[,-c(39,41,42,43,44)],type="response")
dyn_glm_pre_h2<-ifelse(dyn_glm_pre_h2>0.4,1,0)
dyn_tab_glm_h2<-table(dyn_glm_pre_h2,sta_test_data$Heuristic_2)
dyn_con_glm_h2<-confusionMatrix(dyn_tab_glm_h2)
dyn_con_glm_h2


dyn_glm_h3<-glm(Heuristic_3~.,dyn_train_data[,-c(39,40,42,43,44)],family = "binomial")
dyn_glm_pre_h3<-predict(dyn_glm_h3,dyn_test_data[,-c(39,40,42,43,44)],type="response")
dyn_glm_pre_h3<-ifelse(dyn_glm_pre_h3>0.4,1,0)
dyn_tab_glm_h3<-table(dyn_glm_pre_h3,sta_test_data$Heuristic_3)
dyn_con_glm_h3<-confusionMatrix(dyn_tab_glm_h3)
dyn_con_glm_h3

dyn_glm_h4<-glm(Heuristic_4~.,dyn_train_data[,-c(39,40,41,43,44)],family = "binomial")
dyn_glm_pre_h4<-predict(dyn_glm_h4,dyn_test_data[,-c(39,40,41,43,44)],type="response")
dyn_glm_pre_h4<-ifelse(dyn_glm_pre_h4>0.4,1,0)
dyn_tab_glm_h4<-table(dyn_glm_pre_h4,sta_test_data$Heuristic_4)
dyn_con_glm_h4<-confusionMatrix(dyn_tab_glm_h4)
dyn_con_glm_h4

dyn_glm_h5<-glm(Heuristic_5~.,dyn_train_data[,-c(39,40,41,42,44)],family = "binomial")
dyn_glm_pre_h5<-predict(dyn_glm_h5,dyn_test_data[,-c(39,40,41,42,44)],type="response")
dyn_glm_pre_h5<-ifelse(dyn_glm_pre_h5>0.4,1,0)
dyn_tab_glm_h5<-table(dyn_glm_pre_h5,sta_test_data$Heuristic_5)
dyn_con_glm_h5<-confusionMatrix(dyn_tab_glm_h5)
dyn_con_glm_h5


dyn_glm_h6<-glm(Heuristic_6~.,dyn_train_data[,-c(39,40,41,42,43)],family = "binomial")
dyn_glm_pre_h6<-predict(dyn_glm_h6,dyn_test_data[,-c(39,40,41,42,43)],type="response")
dyn_glm_pre_h6<-ifelse(dyn_glm_pre_h6>0.4,1,0)
dyn_tab_glm_h6<-table(dyn_glm_pre_h6,sta_test_data$Heuristic_6)
dyn_con_glm_h6<-confusionMatrix(dyn_tab_glm_h6)
dyn_con_glm_h6


#Random Forest

library(randomForest)
rf_h1<-randomForest(Heuristic_1~.,train_data[,-c(53,54,55,56,57)],ntree=200)
rf_pre_h1<-predict(rf_h1,test_data[,-c(53,54,55,56,57)])
rf_con_h1<-confusionMatrix(rf_pre_h1,test_data$Heuristic_1)

rf_h2<-randomForest(Heuristic_2~.,train_data[,-c(52,54,55,56,57)],ntree=200)
rf_pre_h2<-predict(rf_h2,test_data[,-c(52,54,55,56,57)])
rf_con_h2<-confusionMatrix(rf_pre_h2,test_data$Heuristic_2)

rf_h3<-randomForest(Heuristic_3~.,train_data[,-c(52,53,55,56,57)],ntree=200)
rf_pre_h3<-predict(rf_h3,test_data[,-c(52,53,55,56,57)])
rf_con_h3<-confusionMatrix(rf_pre_h3,test_data$Heuristic_3)

rf_h4<-randomForest(Heuristic_4~.,train_data[,-c(52,53,54,56,57)],ntree=200)
rf_pre_h4<-predict(rf_h4,test_data[,-c(52,53,54,56,57)])
rf_con_h4<-confusionMatrix(rf_pre_h4,test_data$Heuristic_4)

rf_h5<-randomForest(Heuristic_5~.,train_data[,-c(52,53,54,55,57)],ntree=200)
rf_pre_h5<-predict(rf_h5,test_data[,-c(52,53,54,55,57)])
rf_con_h5<-confusionMatrix(rf_pre_h5,test_data$Heuristic_5)

rf_h6<-randomForest(Heuristic_6~.,train_data[,-c(52,53,54,55,56)],ntree=200)
rf_pre_h6<-predict(rf_h6,test_data[,-c(52,53,54,55,56)])
rf_con_h6<-confusionMatrix(rf_pre_h6,test_data$Heuristic_6)

#Random forest with static features
sta_rf_h1<-randomForest(Heuristic_1~.,sta_train_data[,-c(14,15,16,17,18)],ntree=200)
sta_rf_pre_h1<-predict(sta_rf_h1,sta_test_data[,-c(14,15,16,17,18)])
sta_con_h1<-confusionMatrix(sta_rf_pre_h1,sta_test_data$Heuristic_1)

sta_rf_h2<-randomForest(Heuristic_2~.,sta_train_data[,-c(13,15,16,17,18)],ntree=200)
sta_rf_pre_h2<-predict(sta_rf_h2,sta_test_data[,-c(13,15,16,17,18)])
sta_con_h2<-confusionMatrix(sta_rf_pre_h2,sta_test_data$Heuristic_2)

sta_rf_h3<-randomForest(Heuristic_3~.,sta_train_data[,-c(13,14,16,17,18)],ntree=200)
sta_rf_pre_h3<-predict(sta_rf_h3,sta_test_data[,-c(13,14,16,17,18)])
sta_con_h3<-confusionMatrix(sta_rf_pre_h3,sta_test_data$Heuristic_3)

sta_rf_h4<-randomForest(Heuristic_4~.,sta_train_data[,-c(13,14,15,17,18)],ntree=200)
sta_rf_pre_h4<-predict(sta_rf_h4,sta_test_data[,-c(13,14,15,17,18)])
sta_con_h4<-confusionMatrix(sta_rf_pre_h4,sta_test_data$Heuristic_4)

sta_rf_h5<-randomForest(Heuristic_5~.,sta_train_data[,-c(13,14,15,16,18)],ntree=200)
sta_rf_pre_h5<-predict(sta_rf_h5,sta_test_data[,-c(13,14,15,16,18)])
sta_con_h5<-confusionMatrix(sta_rf_pre_h5,sta_test_data$Heuristic_5)

sta_rf_h6<-randomForest(Heuristic_6~.,sta_train_data[,-c(13,14,15,16,17)],ntree=200)
sta_rf_pre_h6<-predict(sta_rf_h6,sta_test_data[,-c(13,14,15,16,17)])
sta_con_h6<-confusionMatrix(sta_rf_pre_h6,sta_test_data$Heuristic_6)



#Dynamic Features

dyn_rf_h1<-randomForest(Heuristic_1~.,dyn_train_data[,-c(40,41,42,43,44)],ntree=200)
dyn_rf_pre_h1<-predict(dyn_rf_h1,dyn_test_data[,-c(40,41,42,43,44)])
dyn_con_h1<-confusionMatrix(sta_rf_pre_h1,dyn_test_data$Heuristic_1)

dyn_rf_h2<-randomForest(Heuristic_2~.,dyn_train_data[,-c(39,41,42,43,44)],ntree=200)
dyn_rf_pre_h2<-predict(dyn_rf_h2,dyn_test_data[,-c(39,41,42,43,44)])
dyn_con_h2<-confusionMatrix(sta_rf_pre_h2,dyn_test_data$Heuristic_2)

dyn_rf_h3<-randomForest(Heuristic_3~.,dyn_train_data[,-c(39,40,42,43,44)],ntree=200)
dyn_rf_pre_h3<-predict(dyn_rf_h3,dyn_test_data[,-c(39,40,42,43,44)])
dyn_con_h3<-confusionMatrix(sta_rf_pre_h3,dyn_test_data$Heuristic_3)

dyn_rf_h4<-randomForest(Heuristic_4~.,dyn_train_data[,-c(39,40,41,43,44)],ntree=200)
dyn_rf_pre_h4<-predict(dyn_rf_h4,dyn_test_data[,-c(39,40,41,43,44)])
dyn_con_h4<-confusionMatrix(sta_rf_pre_h4,dyn_test_data$Heuristic_4)

dyn_rf_h5<-randomForest(Heuristic_5~.,dyn_train_data[,-c(39,40,41,42,44)],ntree=200)
dyn_rf_pre_h5<-predict(dyn_rf_h5,dyn_test_data[,-c(39,40,41,42,44)])
dyn_con_h5<-confusionMatrix(sta_rf_pre_h5,dyn_test_data$Heuristic_5)

dyn_rf_h6<-randomForest(Heuristic_6~.,dyn_train_data[,-c(39,40,41,42,43)],ntree=200)
dyn_rf_pre_h6<-predict(dyn_rf_h6,dyn_test_data[,-c(39,40,41,42,43)])
dyn_con_h6<-confusionMatrix(sta_rf_pre_h6,dyn_test_data$Heuristic_6)



#KNN 
library("class")
help(knn)
colnames(train_data)


library("DMwR")

std_train_data<-preProcess(train_data,method = c("center"))
std_train_data<-predict(std_train_data,train_data)

std_test_data<-preProcess(test_data,method = c("center"))
std_test_data<-predict(std_test_data,test_data)


knn_h1<-knn(train =train_data[,1:52],
            test = test_data[,1:52],
            cl=train_data$Heuristic_1,
            k=3)
knn_tab_h1<-table(knn_h1,test_data$Heuristic_1)
knn_con_h1<-confusionMatrix(knn_tab_h1) 

colnames(train_data)

h2_rows=c(1:51,53)
knn_h2<-knn(train =train_data[,h2_rows],
            test = test_data[,h2_rows],
            cl=train_data$Heuristic_2,
            k=3)

knn_tab_h2<-table(knn_h2,test_data$Heuristic_2)
knn_con_h2<-confusionMatrix(knn_tab_h2)

h3_rows=c(1:51,54)
knn_h3<-knn(train =train_data[,h3_rows],
            test = test_data[,h3_rows],
            cl=train_data$Heuristic_3,
            k=3)

knn_tab_h3<-table(knn_h3,test_data$Heuristic_3)
knn_con_h3<-confusionMatrix(knn_tab_h3)

h4_rows=c(1:51,55)
knn_h4<-knn(train =train_data[,h4_rows],
            test = test_data[,h4_rows],
            cl=train_data$Heuristic_4,
            k=5)

knn_tab_h4<-table(knn_h4,test_data$Heuristic_4)
knn_con_h4<-confusionMatrix(knn_tab_h4)




h5_rows=c(1:51,56)
knn_h5<-knn(train =train_data[,h5_rows],
            test = test_data[,h5_rows],
            cl=train_data$Heuristic_5,
            k=3)

knn_tab_h5<-table(knn_h5,test_data$Heuristic_5)
knn_con_h5<-confusionMatrix(knn_tab_h5)



h6_rows=c(1:51,57)
knn_h6<-knn(train =train_data[,h6_rows],
            test = test_data[,h6_rows],
            cl=train_data$Heuristic_6,
            k=3)

knn_tab_h6<-table(knn_h6,test_data$Heuristic_6)
knn_con_h6<-confusionMatrix(knn_tab_h6)




#Static features
colnames(train_data)
sta_h1_cols=c(1:13,52)
knn_sta_h1<-knn(train =train_data[,sta_h1_cols],
            test = test_data[,sta_h1_cols],
            cl=train_data$Heuristic_1,
            k=3)

knn_sta_tab_h1<-table(knn_sta_h1,test_data$Heuristic_1)
knn_con_sta_h1<-confusionMatrix(knn_tab_h1)


sta_h2_cols=c(1:13,53)
knn_sta_h2<-knn(train =train_data[,sta_h2_cols],
                test = test_data[,sta_h2_cols],
                cl=train_data$Heuristic_2,
                k=3)

knn_sta_tab_h2<-table(knn_sta_h2,test_data$Heuristic_2)
knn_con_sta_h2<-confusionMatrix(knn_tab_h2)

sta_h3_cols=c(1:13,54)
knn_sta_h3<-knn(train =train_data[,sta_h3_cols],
                test = test_data[,sta_h3_cols],
                cl=train_data$Heuristic_3,
                k=3)

knn_sta_tab_h3<-table(knn_sta_h3,test_data$Heuristic_3)
knn_con_sta_h3<-confusionMatrix(knn_tab_h3)


sta_h4_cols=c(1:13,55)
knn_sta_h4<-knn(train =train_data[,sta_h4_cols],
                test = test_data[,sta_h4_cols],
                cl=train_data$Heuristic_4,
                k=3)

knn_sta_tab_h4<-table(knn_sta_h4,test_data$Heuristic_4)
knn_con_sta_h4<-confusionMatrix(knn_tab_h4)


sta_h5_cols=c(1:13,56)
knn_sta_h5<-knn(train =train_data[,sta_h5_cols],
                test = test_data[,sta_h5_cols],
                cl=train_data$Heuristic_5,
                k=3)

knn_sta_tab_h5<-table(knn_sta_h5,test_data$Heuristic_5)
knn_con_sta_h5<-confusionMatrix(knn_tab_h5)

sta_h6_cols=c(1:13,57)
knn_sta_h6<-knn(train =train_data[,sta_h6_cols],
                test = test_data[,sta_h6_cols],
                cl=train_data$Heuristic_6,
                k=3)

knn_sta_tab_h6<-table(knn_sta_h6,test_data$Heuristic_6)
knn_con_sta_h6<-confusionMatrix(knn_tab_h6)




#Dynamic features
colnames(train_data)
dyn_h1_cols=c(14:51,52)
knn_dyn_h1<-knn(train =train_data[,dyn_h1_cols],
                test = test_data[,dyn_h1_cols],
                cl=train_data$Heuristic_1,
                k=3)

knn_dyn_tab_h1<-table(knn_dyn_h1,test_data$Heuristic_1)
knn_con_dyn_h1<-confusionMatrix(knn_dyn_tab_h1)

dyn_h2_cols=c(14:51,53)
knn_dyn_h2<-knn(train =train_data[,dyn_h2_cols],
                test = test_data[,dyn_h2_cols],
                cl=train_data$Heuristic_2,
                k=3)

knn_dyn_tab_h2<-table(knn_dyn_h2,test_data$Heuristic_2)
knn_con_dyn_h2<-confusionMatrix(knn_dyn_tab_h2)


dyn_h3_cols=c(14:51,54)
knn_dyn_h3<-knn(train =train_data[,dyn_h3_cols],
                test = test_data[,dyn_h3_cols],
                cl=train_data$Heuristic_3,
                k=3)

knn_dyn_tab_h3<-table(knn_dyn_h3,test_data$Heuristic_3)
knn_con_dyn_h3<-confusionMatrix(knn_dyn_tab_h3)

dyn_h4_cols=c(14:51,55)
knn_dyn_h4<-knn(train =train_data[,dyn_h4_cols],
                test = test_data[,dyn_h4_cols],
                cl=train_data$Heuristic_4,
                k=3)

knn_dyn_tab_h4<-table(knn_dyn_h4,test_data$Heuristic_4)
knn_con_dyn_h4<-confusionMatrix(knn_dyn_tab_h4)



dyn_h5_cols=c(14:51,56)
knn_dyn_h5<-knn(train =train_data[,dyn_h5_cols],
                test = test_data[,dyn_h5_cols],
                cl=train_data$Heuristic_5,
                k=3)

knn_dyn_tab_h5<-table(knn_dyn_h5,test_data$Heuristic_5)
knn_con_dyn_h5<-confusionMatrix(knn_dyn_tab_h5)


dyn_h6_cols=c(14:51,57)
knn_dyn_h6<-knn(train =train_data[,dyn_h6_cols],
                test = test_data[,dyn_h6_cols],
                cl=train_data$Heuristic_6,
                k=3)

knn_dyn_tab_h6<-table(knn_dyn_h6,test_data$Heuristic_6)
knn_con_dyn_h6<-confusionMatrix(knn_dyn_tab_h6)















