library(ggpubr)#histogram
library(splitstackshape)#split dataset
library(DMwR2)#check local outlier
library(caret) #train() function and confusion matrix
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms and visualizations
library(tidyverse) # data transformation
library(dplyr) # used for data manipulation
library(janitor) # examining and cleaning dirty data

#read file
statisfaction<-read.csv("Personality_Test.csv")

#study data patterns
str(statisfaction)
summary(statisfaction)

#check missing data
print(table(is.na(statisfaction)))

#remove column Timestamp
statisfaction<-statisfaction[,-1]

#convert the data of all columns except personality, Satisfaction, Age and time spent into numeric
for(i in 2:ncol(statisfaction)){
  if(i==7||i==8||i==12){next}
  data<-unique(statisfaction[,i])
  cat("Unique variables in column",i,":",data,"\n")
  statisfaction[,i]<-as.factor(statisfaction[,i])
}
statisfaction[13,8]<-5
statisfaction[15,8]<-16
statisfaction[22,8]<-6

#convert to mean value of respondent's time interval given
statisfaction[8,8]<-5.5
statisfaction[16,8]<-6.5
statisfaction[43,8]<-0.5
statisfaction[69,8]<-0.5
statisfaction[88,8]<-0.5

#convert column 8 to numeric
statisfaction[,8]<-as.numeric(statisfaction[,8])

#study column 8's data distribution
ggplot(statisfaction, aes(x=statisfaction[,8])) +
  geom_density() +
  geom_vline(aes(xintercept=mean(statisfaction[,8], na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

#impute mean value for respondent who give a invalid answer
ave_time_spent<-round(mean(statisfaction$How.many.time.spent.for.working.or.studying.at.home...Hours.per.day., na.rm = TRUE),digits = 2)
personality_col8<- ifelse(is.na(statisfaction[,8]),ave_time_spent,statisfaction[,8])
statisfaction$How.many.time.spent.for.working.or.studying.at.home...Hours.per.day.<-personality_col8

#study transformed data patterns
str(statisfaction)

# Rename columns-
colnames(statisfaction)[7] <- "Personality"
colnames(statisfaction)[8] <- "work_study_time"
colnames(statisfaction)[9] <- "Internet_speed"
colnames(statisfaction)[10] <- "enjoy_wfh"
colnames(statisfaction)[11] <- "home_outdoor"
colnames(statisfaction)[12] <- "statisfy"
colnames(statisfaction)[13] <- "communication"

# duplicate column for personalities
statisfaction <- cbind(statisfaction, rep(statisfaction[7],3))
# rename duplicated colums
colnames(statisfaction)[7] <- "introvert_extravert"#IE
colnames(statisfaction)[14] <- "intuitive_observant"# NS
colnames(statisfaction)[15] <- "thinking_feeling"#TF
colnames(statisfaction)[16] <- "judging_prospecting" #JP

# rename duplicated colums
colnames(statisfaction)[7] <- "introvert_extravert"#IE
colnames(statisfaction)[14] <- "intuitive_observant"# NS
colnames(statisfaction)[15] <- "thinking_feeling"#TF
colnames(statisfaction)[16] <- "judging_prospecting" #JP

## change the statisfaction to statisfy and not statisfy
statisfaction$statisfy[statisfaction$statisfy=="Sad"] <- "Not statisfy"
statisfaction$statisfy[statisfaction$statisfy=="Stressful"] <- "Not statisfy"
statisfaction$statisfy[statisfaction$statisfy=="Disappointed"] <- "Not statisfy"
statisfaction$statisfy[statisfaction$statisfy=="Neutral"] <- "Neutral"
statisfaction$statisfy[statisfaction$statisfy=="Relieved"] <- "Statisfy"
statisfaction$statisfy[statisfaction$statisfy=="Happy"] <- "Statisfy"
statisfaction$statisfy[statisfaction$statisfy=="Motivated"] <- "Statisfy"

## change/ break down the personality
### I
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Architect (INTJ)"] <- "introvert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Logician (INTP)"] <- "introvert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Advocate (INFJ)"] <- "introvert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Mediator (INFP)"] <- "introvert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Logistician (ISTJ)"] <- "introvert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Defender (ISFJ)"] <- "introvert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Virtuoso (ISTP)"] <- "introvert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Adventurer (ISFP)"] <- "introvert"
#E
#statisfaction$introvert_extravert[statisfaction$introvert_extravert==""] <- "extrovert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Consul (ESFJ)"] <- "extrovert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Campaigner (ENFP)"] <- "extrovert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Entertainer (ESFP)"] <- "extrovert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Commander (ENTJ)"] <- "extrovert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Entrepreneur (ESTP)"] <- "extrovert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Protagonist (ENFJ)"] <- "extrovert"
statisfaction$introvert_extravert[statisfaction$introvert_extravert=="Executive (ESTJ)"] <- "extrovert"

#N
#statisfaction$intuitive_observant[statisfaction$intuitive_observant==""] <- "intuitive"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Mediator (INFP)"] <- "intuitive"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Architect (INTJ)"] <- "intuitive"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Logician (INTP)"] <- "intuitive"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Commander (ENTJ)"] <- "intuitive"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Debater (ENTP)"] <- "intuitive"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Advocate (INFJ)"] <- "intuitive"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Protagonist (ENFJ)"] <- "intuitive"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Campaigner (ENFP)"] <- "intuitive"

#S
#statisfaction$intuitive_observant[statisfaction$intuitive_observant==""] <- "observant"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Logistician (ISTJ)"] <- "observant"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Defender (ISFJ)"] <- "observant"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Executive (ESTJ)"] <- "observant"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Consul (ESFJ)"] <- "observant"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Virtuoso (ISTP)"] <- "observant"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Adventurer (ISFP)"] <- "observant"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Entertainer (ESFP)"] <- "observant"
statisfaction$intuitive_observant[statisfaction$intuitive_observant=="Entrepreneur (ESTP)"] <- "observant"


#T
#statisfaction$thinking_feeling[statisfaction$thinking_feeling==""] <- "thinking"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Architect (INTJ)"] <- "thinking"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Logician (INTP)"] <- "thinking"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Commander (ENTJ)"] <- "thinking"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Debater (ENTP)"] <- "thinking"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Logistician (ISTJ)"] <- "thinking"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Executive (ESTJ)"] <- "thinking"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Entrepreneur (ESTP)"] <- "thinking"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Virtuoso (ISTP)"] <- "thinking"

# F
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Advocate (INFJ)"] <- "feeling"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Mediator (INFP)"] <- "feeling"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Campaigner (ENFP)"] <- "feeling"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Protagonist (ENFJ)"] <- "feeling"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Defender (ISFJ)"] <- "feeling"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Consul (ESFJ)"] <- "feeling"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Adventurer (ISFP)"] <- "feeling"
statisfaction$thinking_feeling[statisfaction$thinking_feeling=="Entertainer (ESFP)"] <- "feeling"

#J
#judging_prospecting
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Architect (INTJ)"] <- "judging"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Commander (ENTJ)"] <- "judging"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Advocate (INFJ)"] <- "judging"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Protagonist (ENFJ)"] <- "judging"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Logistician (ISTJ)"] <- "judging"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Executive (ESTJ)"] <- "judging"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Defender (ISFJ)"] <- "judging"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Consul (ESFJ)"] <- "judging"

#P
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Logician (INTP)"] <- "prospecting"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Debater (ENTP)"] <- "prospecting"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Mediator (INFP)"] <- "prospecting"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Campaigner (ENFP)"] <- "prospecting"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Virtuoso (ISTP)"] <- "prospecting"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Adventurer (ISFP)"] <- "prospecting"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Entertainer (ESFP)"] <- "prospecting"
statisfaction$judging_prospecting[statisfaction$judging_prospecting=="Entrepreneur (ESTP)"] <- "prospecting"

#transform data columns 7,14,15,16 to factor type
statisfaction[,7]<-as.factor(statisfaction[,7])
statisfaction[,14]<-as.factor(statisfaction[,14])
statisfaction[,15]<-as.factor(statisfaction[,15])
statisfaction[,16]<-as.factor(statisfaction[,16])

#store names of each variables
conames<-names(statisfaction)

#Data visualisation: graph of satisfaction of work/study from home among all variables
m<-1
n<-m
while(m <= ncol(statisfaction)){
  if(m==12){
    m<-m+1
    n<-m
  }else{
  df1<-data.frame(table(statisfaction[,12],statisfaction[,n]))
  
  if(n==8||n==1){
    bp<-ggplot(df1, aes(fill=df1[,1], y=df1[,3], x=df1[,2])) +
      ggtitle('Satisfaction count by variable',m) +
      xlab(conames[m]) +
      ylab('Satisfaction Count') +
      labs(fill="satisfaction")+
      geom_bar(position='dodge', stat='identity')+
      geom_text(aes(label = df1[,2]),nudge_y = .5,colour='purple')+
      geom_text(aes(label = signif(df1[,3])))+
      coord_flip()
  }else{
    bp<-ggplot(df1, aes(fill=df1[,1], y=df1[,3], x=df1[,2])) +
      ggtitle('Satisfaction count by variable',m) +
      xlab(conames[m]) +
      ylab('Satisfaction Count') +
      labs(fill="satisfaction")+
      geom_bar(position='dodge', stat='identity')+
      geom_text(aes(label = signif(df1[,3])))+
      coord_flip()
  }
  
  print(bp)
  Sys.sleep(2)

  m<-m+1
  n<-m
  }
}

#create numeric data set used for check the outlier
num_sat_Data<-statisfaction
num_sat_Data[,12]<-as.numeric(as.factor(num_sat_Data[,12]))
index<-sapply(num_sat_Data, is.factor)
num_sat_Data[index] <-lapply(num_sat_Data[index], function(x) as.numeric(as.factor(x)))

#check local outlier
outlier.scores<-lofactor(num_sat_Data,k=2)
plot(density(outlier.scores))

#print top 6 outliers for classifiers while top 5 outliers is removed for clustering 
outliers1<-order(outlier.scores,decreasing = T)[1:6]
outler_clus<-order(outlier.scores,decreasing = T)[1:5]
print(outliers1)

#transform target variable to factor type
statisfaction[,12]<-as.factor(statisfaction[,12])

#remove outliers
statisfaction_clus<-statisfaction[-(c(outler_clus)),]
statisfaction<-statisfaction[-(c(outliers1)),]

#data splitting-----------------------------------------------------------
#create data set with the target column removed that will 
#be used for model training and testing purpose

#make this example reproducible
set.seed(3003)
#use 70% of dataset as training set and 30% as test set
train_test_data = sort(sample(nrow(statisfaction), nrow(statisfaction)*.7))
train<-statisfaction[train_test_data,]
test<-statisfaction[-train_test_data,]
#---------------------------------------------------------------------------------
#setup trainControl
trainctrl<-trainControl(method="repeatedcv",number=10,repeats = 5,sampling='down')

#setup model tuning(data modelling)
grid_radial<-expand.grid(sigma=c(.01,.015,0.2),C=c(0.25,0.5,0.65,1,1.25,1.5))
grid_radial

#train svm radial basis
svm_radial<-train(statisfy~.,data = statisfaction,method="svmRadial",trControl=trainctrl,
                  preProcess=c("center"),
                  tuneGrid=grid_radial,tuneLength=10)
svm_radial

#tuning for svm linear
grid_linear<-expand.grid(C=c(0.25,0.5,0.75,1,1.25,1.5,1.75,2))
grid_linear

#train svm linear
svm_linear<-train(statisfy~.,data = statisfaction,method="svmLinear",trControl=trainctrl,
                  preProcess=c("center"),
                  tuneGrid=grid_linear)
svm_linear

#tuning for svm polynomial
grid_poly<-expand.grid(C=c(0.25,0.5,0.75,1,1.25,1.5,1.75,2),degree=c(1,2,3),scale=c(.001,0.01,.1))
grid_poly

#train svm polynomial
svm_poly<-train(statisfy~.,data = statisfaction,method="svmPoly",trControl=trainctrl,
                preProcess=c("center"),
                tunelength=3)
svm_poly

#prediction
pred1<-predict(svm_radial,test)
pred2<-predict(svm_poly,test)
pred3<-predict(svm_linear,test)

#confusion matrix
cm1<-confusionMatrix(pred1,test$statisfy)
print(cm1)
cm2<-confusionMatrix(pred2,test$statisfy)
print(cm2)
cm3<-confusionMatrix(pred3,test$statisfy)
print(cm3)

class_satisfaction<-c("Neutral","Not statisfy","Statisfy")

#compute f1,recall,sensitivity & precision for rbf
f1_rbf<-cm1[["byClass"]][ , "F1"]
Precision_rbf<-cm1[["byClass"]][ , "Precision"]
Recall_rbf<-cm1[["byClass"]][ , "Recall"]
Sensitivity_rbf<-cm1[["byClass"]][ , "Sensitivity"]
metrics_rbf<-data.frame(class_satisfaction,f1_rbf,Precision_rbf,Recall_rbf,Sensitivity_rbf)

#compute f1,recall,sensitivity & precision for poly
f1_poly<-cm2[["byClass"]][ , "F1"]
Precision_poly<-cm2[["byClass"]][ , "Precision"]
Recall_poly<-cm2[["byClass"]][ , "Recall"]
Sensitivity_poly<-cm2[["byClass"]][ , "Sensitivity"]
metrics_poly<-data.frame(class_satisfaction,f1_poly,Precision_poly,Recall_poly,Sensitivity_poly)

#compute f1,recall,sensitivity & precision for linear
f1_linear<-cm3[["byClass"]][ , "F1"]
Precision_linear<-cm3[["byClass"]][ , "Precision"]
Recall_linear<-cm3[["byClass"]][ , "Recall"]
Sensitivity_linear<-cm3[["byClass"]][ , "Sensitivity"]
metrics_linear<-data.frame(class_satisfaction,f1_linear,Precision_linear,Recall_linear,Sensitivity_linear)

#feature Importance
imp_rbf<- varImp(svm_radial, scale=FALSE)
imp_poly<- varImp(svm_poly, scale=FALSE)
imp_linear<- varImp(svm_linear, scale=FALSE)
plot(imp_linear)
plot(imp_rbf)
plot(imp_poly)

#--------------------------------------------------------------------------------
## Build model (decision tree)
library(rpart)
library(rpart.plot)
decision_tree_model <- rpart(statisfy~., data = train, method = 'class')
rpart.plot(decision_tree_model, extra = 100)

## Make prediction using Decision Tree
#test <- subset(test, select = -statisfy)
predict_decisionTree <-predict(decision_tree_model, test, type = 'class')

## to see who is statisfy , neutral and not statisfy
table_matrix_DT <- table(test$statisfy, predict_decisionTree)
table_matrix_DT

## measure performance
accuracy_DT <- sum(diag(table_matrix_DT)) / sum(table_matrix_DT)
print(paste('Accuracy for test', accuracy_DT))
cm_DT<-confusionMatrix(predict_decisionTree,test$statisfy)
cm_DT

#compute f1,recall,sensitivity & precision for decision tree
f1_DT<-cm_DT[["byClass"]][ , "F1"]
Precision_DT<-cm_DT[["byClass"]][ , "Precision"]
Recall_DT<-cm_DT[["byClass"]][ , "Recall"]
Sensitivity_DT<-cm_DT[["byClass"]][ , "Sensitivity"]
metrics_DT<-data.frame(class_satisfaction,f1_DT,Precision_DT,Recall_DT,Sensitivity_DT)

#--------------------------------------
# Feature importance- (before consturct model)
library(Boruta)
#statisfaction$statisfy <- as.factor(statisfaction$statisfy)
boruta_output <- Boruta(statisfy ~ ., data=statisfaction, doTrace=0) 
names(boruta_output)

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

# Variable Importance Scores
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 
#-------------------------------------------------------------
#clustering
set.seed(10)

# use the kmeans() function to form the clusters
clus_result <- kmeans(statisfaction_clus[c(1,8)], 3, nstart = 20) # apply k-means algorithm using numeric attributes and with k=3(no. of required clusters)
clus_result

# gives no. of records in each cluster
clus_result$size

# gives value of cluster center data point value(3 centers for k=3)
clus_result$centers

#gives cluster vector showing the cluster where each record falls
clus_result$cluster

# the total within-cluster sum of squares
clus_result$tot.withinss

# Plot to see all attribute combinations
plot(statisfaction_clus[,], col= clus_result$cluster) 

clus_result$cluster <- as.factor(clus_result$cluster)

# forming a table with the Satisfy column of the original data
table(statisfaction_clus$statisfy,clus_result$cluster)
cm <- table(as.integer(statisfaction_clus$statisfy),clus_result$cluster)
CM<-confusionMatrix(cm)
CM

# print the accuracy of the clustering
accuracy = sum(diag(cm)) / sum(cm) 

#confusion matrix
sprintf("The accuracy of the model is %.4f" , accuracy)

#compute f1,recall,sensitivity & precision
f1_kmeans<-CM[["byClass"]][ , "F1"]
Precision_kmeans<-CM[["byClass"]][ , "Precision"]
Recall_kmeans<-CM[["byClass"]][ , "Recall"]
Sensitivity_kmeans<-CM[["byClass"]][ , "Sensitivity"]
metrics_kmeans<-data.frame(class_satisfaction,f1_kmeans,Precision_kmeans,Recall_kmeans,Sensitivity_kmeans)

# plot the cluster plot graph
ggplot(statisfaction_clus, aes(Age, `work_study_time`, col = clus_result$cluster)) + geom_point() + ggtitle("Personality Test Cluster Plot")