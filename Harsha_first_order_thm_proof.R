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
static_features <- sprintf("Sta-%s",seq(1:14))
static_features

#Dynamic Features col name creation
dynamic_features<-sprintf("Dyn-%s",seq(15:53))
dynamic_features

#Huerisic features col name creation
heuristic_tar<-sprintf("Heuristic-%s",seq(1:6))
heuristic_tar

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

target<-subset(new_data,select = c(52,53,54,55,56,57))
preObj <- preProcess(new_data[, -c(52,53,54,55,56,57)], method=c("center", "scale"))

newData <- predict(preObj, new_data[, -c(52,53,54,55,56,57)])

std_data<-cbind(newData,target)


summary(newData)
train_rows<-sample(nrow(new_data),0.8*nrow(new_data))              
train_data<-new_data[train_rows,]
test_data<-new_data[-train_rows,]

# std_train_rows<-sample(nrow(std_data),0.8*nrow(std_data))              
# std_train_data<-std_data[train_rows,]
# std_test_data<-std_data[-train_rows,]
# 
# std_glm_model<-glm(std_train_data$`Heuristic-1`~.,data=std_train_data[,-c(53:57)],family = "binomial")
# std_glm_pre<-predict(std_glm_model,std_test_data[,-c(53:57)],type = "response")
# 
# 
# std_glm_pre<-ifelse(std_glm_pre>0.5,1,0)
# std_tab1<-table(std_glm_pre,std_test_data$`Heuristic-1`)
# std_met1<-confusionMatrix(std_tab1)
# std_met1
# 



#Model Building

# summary(ik)
# 
glm_model1<-glm(train_data$`Heuristic-1`~.,data=train_data[,-c(53:57)],family = "binomial")
glm_pre1<-predict(glm_model1,test_data[,-c(53:57)],type = "response")
glm_pre1<-ifelse(glm_pre1>0.5,1,0)
tab1<-table(glm_pre1,test_data$`Heuristic-1`)
met1<-confusionMatrix(tab1)
met1
# 
# colnames(train_data)
# 
# 
glm_model2<-glm(train_data$`Heuristic-2`~.,data=train_data[,-c(53:57)],family = "binomial")
glm_pre2<-predict(glm_model2,test_data[,-c(53:57)],type = "response")
glm_pre2<-ifelse(glm_pre2>0.5,1,0)
 tab2<-table(glm_pre2,test_data$`Heuristic-2`)
met2<-confusionMatrix(tab2)
met2 





log_reg<-function(a,b){
  as.factor(a[,54])
  model1<-glm(a[,54]~.,data=a,family = "binomial")
  pre1<-predict(model1,b,type="response")
  
  pre1<-ifelse(pre1>0.5,1,0)
  print(pre1)
  tab1<-table(pre1,b[,54])
  print(tab1)
  confusionMatrix(tab1)
}


log_reg(train_data,test_data)



colnames(train_data)
colnames(test_data)

model <- list()
dv<-colnames(train_data[,52:57])
dv
for (i in p) {
  f<-formula(paste(i,"~","."))
  
  model[[i]] = glm(f,train_data,family = "binomial") 
}
  
lapply(model, summary)
model[1]

  c<-predict(model2,test_data,type="response")
c<-ifelse(c>0.5,1,0)

confusionMatrix(c,test_data$`Heuristic-1`)




p<-c("`Heuristic-1`" ,"`Heuristic-2`", "`Heuristic-3`" ,"`Heuristic-4`" ,"`Heuristic-5`" ,"`Heuristic-6`")


train_data$`Heuristic-1`


for(o in model){
  summary(o)
}