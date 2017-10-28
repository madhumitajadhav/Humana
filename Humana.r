###############################################################

library(data.table)
humana<-read.csv("D:/UTD/Competitions/Humana/humana.csv")
names(humana)

humana<-humana[complete.cases(humana), ]

#Removed all variables having zero varience
humana1<-humana[,-c(32,34,36,41,59,63,68,83,84,89,350,572,573,574,575,576,596,597,598,601,604,607,614,616,617,618,620,627,630,631,634,637,638,640,641)]

# Removed all POT and variables having zero variance  
humana2<-humana[,-c(32,34,36,39,40,41,59,63,66,67,68,83,84,89,155:162,187:194,220:227,260:275,350,363:652)] 

# Removing Null Values From data
humana3<-humana2[complete.cases(humana2), ]

humana3[which(humana3$READMISSIONS==0),2]<-0
humana3[which(humana3$READMISSIONS!=0),2]<-1
humana3$READMISSIONS<-as.factor(humana3$READMISSIONS)

humana4<-humana3[,c(1:92,577:594)]
humana4[which(humana4$ADMISSIONS==0),1]<-0
humana4[which(humana4$ADMISSIONS!=0),1]<-1
humana4$ADMISSIONS<-as.factor(humana4$ADMISSIONS)


# Analysing CON factors
con<-humana3[,c(2,93:307)]
fit <- glm(con$READMISSIONS~.,data=con,family=binomial())
summary(fit)

# Analysing Rx factors
rx<-humana3[,c(2,308:576)]
fit <- glm(rx$READMISSIONS~.,data=rx,family=binomial())
summary(fit)

# For Testing
# humana4<-humana3[,c(1:92,577:594)]
# humana4$READMISSIONS<-as.numeric(humana4$READMISSIONS)
# 
# humana4[which(humana4$READMISSIONS==0),2]<-0
# humana4[which(humana4$READMISSIONS!=0),2]<-1
# humana4$READMISSIONS<-as.factor(humana4$READMISSIONS)
# 
# humana4[which(humana4$ADMISSIONS==0),1]<-0
# humana4[which(humana4$ADMISSIONS!=0),1]<-1
# humana4$ADMISSIONS<-as.factor(humana4$ADMISSIONS)
# 

library(caret)

############################## Exploratory Analysis ##########################

ad<-humana4$ADMISSIONS
read<-humana4$READMISSIONS
age<-humana4$AGE
month<-humana4$MONTHS_2016
los<-humana4$Length_residence
READMISSION<-humana4$READMISSIONS

install.packages(c("rgl", "car"))
# install.packages("scatterplot3d")
library(scatterplot3d)
library(rgl)
library(car)

scatter3d(x = age, y = month, z = los,point.col = "blue", surface=FALSE)
scatter3d(x = age, y = month, z = los, groups = as.factor(READMISSION))
scatter3d(x = age, y = month, z = los, groups = as.factor(READMISSION),grid = FALSE)
scatter3d(x = age, y = month, z = los, groups = as.factor(READMISSION),grid = FALSE, fit = "smooth")
scatter3d(x = age, y = month, z = los, groups = as.factor(READMISSION),grid = FALSE, surface = FALSE)


################################################################################
# Partitionaing Data

inTrain <- createDataPartition(y=humana4$ADMISSIONS, p=0.7, list=FALSE)
training <- humana4[inTrain, ]
testing <- humana4[-inTrain,]

row.names(training) <- 1:nrow(training)
row.names(a) <- 1:nrow(a)

# library(devtools)
# library(woe)
# library(riv)

library(devtools)
install_github("riv","tomasgreif")
install_github("woe","tomasgreif")
library(woe)
library(riv) 

IV<-iv.mult(training,y="ADMISSIONS",TRUE)
var<-IV[which(IV$InformationValue>0.01),]
var1<-var[which(var$InformationValue<1.0),]
final_var<-var1$Variable
final_var
iv.plot.summary(var1)
x_train<-training[final_var]

IV<-iv.mult(training,y="ADMISSIONS",TRUE)
var<-IV[which(IV$InformationValue>0.01),]
var1<-var[which(var$InformationValue<0.8),]
final_var<-var1$Variable
final_var
x_train<-training[final_var]
iv.plot.summary(var1)

set.seed(1777)
random_forest=randomForest(training$ADMISSIONS~.,data=training,ntree=500,importance=TRUE)
random_forest

train<-training[,c("ADMISSIONS","RECON_MA_RISK_SCORE_NBR","RECON_RX_RISK_SCORE_NBR","FLAG_CV_HDZ_2015","FLAG_CV_CIR_2015","FLAG_CV_CIR_2014","FLAG_CV_HDZ_2014"            
            ,"FLAG_HYPERTENSION_2014","FLAG_CV_CAD_2015","FLAG_RES_INF_2015" ,"AGE","FLAG_Diab_Complications_2015","FLAG_CKD_2015"               
            ,"FLAG_RENAL_2015","FLAG_DEPRESSION_2015","FLAG_MUSCUL_BN_2015","COL_2015","Length_residence"            
            ,"LIS","FLAG_HYPERTENSION_2015","FLAG_ARTH_2015" ,"Est_income","ORIG_REAS_ENTITLE_CD","CDC_HBAGOOD_GAP_2014"        
            ,"CDC_2014","Est_Net_worth","CDC_EYE_GAP_2014","FLAG_CANCER_2015","Decile_struggle_Med_lang","Num_person_household"        
            ,"Est_BMI_decile", "MINOR_GEOGRAPHY","DUAL","INSTITUTIONAL","DIAB_PASS_2015")]

test<-testing[,c("ADMISSIONS","RECON_MA_RISK_SCORE_NBR","RECON_RX_RISK_SCORE_NBR","FLAG_CV_HDZ_2015","FLAG_CV_CIR_2015","FLAG_CV_CIR_2014","FLAG_CV_HDZ_2014"            
                   ,"FLAG_HYPERTENSION_2014","FLAG_CV_CAD_2015","FLAG_RES_INF_2015" ,"AGE","FLAG_Diab_Complications_2015","FLAG_CKD_2015"               
                   ,"FLAG_RENAL_2015","FLAG_DEPRESSION_2015","FLAG_MUSCUL_BN_2015","COL_2015","Length_residence"            
                   ,"LIS","FLAG_HYPERTENSION_2015","FLAG_ARTH_2015" ,"Est_income","ORIG_REAS_ENTITLE_CD","CDC_HBAGOOD_GAP_2014"        
                   ,"CDC_2014","Est_Net_worth","CDC_EYE_GAP_2014"            
                   ,"FLAG_CANCER_2015","Decile_struggle_Med_lang","Num_person_household"        
                   ,"Est_BMI_decile", "MINOR_GEOGRAPHY","DUAL","INSTITUTIONAL","DIAB_PASS_2015")]
### SVM ###
library(e1071)
svm.model <- svm(train$ADMISSIONS ~ ., data = train, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, test[,-1])
table(pred = svm.pred, true = test[,1])

### Rpart ###
library(rpart)
rpart.model <- rpart(train$ADMISSIONS ~ ., data = train)
rpart.pred <- predict(rpart.model, test[,-1], type = "class")
table(pred = rpart.pred, true = test[,1])

###
library(randomForest)
set.seed(1777)
random_forest=randomForest(train$ADMISSIONS~.,data=train,ntree=500,importance=TRUE)
random_forest

cforest(ADMISSIONS ~ ., data=train, controls=cforest_control(mtry=2, mincriterion=0))


# Call:
#   randomForest(formula = train$ADMISSIONS ~ ., data = train, ntree = 500,      importance = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 5
# 
# OOB estimate of  error rate: 18.23%
# Confusion matrix:
#       0   1 class.error
# 0 20458 303  0.01459467
# 1  4349 407  0.91442389

plot(random_forest,main="Random Forest: Error Rate vs Number of Trees")

imp=importance(random_forest)
impL=imp[,c(3,4)]
imp.ma=as.matrix(impL)
imp.df=data.frame(imp.ma)

write.csv(imp.df, "D:/UTD/Competitions/Humana/imp.df.csv", row.names=TRUE)
imp.df.csv=read.csv("D:/UTD/Competitions/Humana/imp.df.csv",header=TRUE)

colnames(imp.df.csv)=c("Variable","MeanDecreaseAccuracy","MeanDecreaseGini")
imp.sort =  imp.df.csv[order(-imp.df.csv$MeanDecreaseAccuracy),] 

imp.sort = transform(imp.df.csv, 
                     Variable = reorder(Variable, MeanDecreaseAccuracy))

VIP=ggplot(data=imp.sort, aes(x=Variable, y=MeanDecreaseAccuracy)) + 
  ylab("Mean Decrease Accuracy")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.75)+ 
  coord_flip()+theme_few() 

imp.sort.Gini <- transform(imp.df.csv, 
                           Variable = reorder(Variable, MeanDecreaseGini))

VIP.Gini=ggplot(data=imp.sort.Gini, aes(x=Variable, y=MeanDecreaseGini)) + 
  ylab("Mean Decrease Gini")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.75)+ 
  coord_flip()+theme_few()

library(ggthemes)
library(gridExtra)
library(ggplot2)
library(grid)

VarImpPlot=arrangeGrob(VIP, VIP.Gini,ncol=2)
grid.draw(VarImpPlot)

test_predictions = predict(random_forest, newdata=test)
confusionMatrix(test_predictions,test$ADMISSIONS)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0           8759 1868
# 1           138  170
# 
# Accuracy : 0.8166          
# 95% CI : (0.8092, 0.8238)
# No Information Rate : 0.8136          
# P-Value [Acc > NIR] : 0.2199          
# 
# Kappa : 0.1009          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.98449         
# Specificity : 0.08342         
# Pos Pred Value : 0.82422         
# Neg Pred Value : 0.55195         
# Prevalence : 0.81363         
# Detection Rate : 0.80101         
# Detection Prevalence : 0.97183         
# Balanced Accuracy : 0.53395         
# 
# 'Positive' Class : 0            

###############################

library(e1071)
svm.model <- svm(training$READMISSIONS ~ ., data = training, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset_2015[,-23])

###############################
# Logistic Regression #
training$READMISSIONS<-as.factor(training$READMISSIONS)
testing$READMISSIONS<-as.factor(testing$READMISSIONS)

fit <- glm(training$READMISSIONS~.,data=training,family=binomial())
summary(fit)

pred_prob<-predict (fit, newdata=testing, type="response")

library (ROCR)
pred <- prediction(pred_prob, testing$READMISSIONS)
performance(pred, 'auc')
roc <- performance (pred,"tpr","tnr")
plot (roc)
perf <-as.data.frame(cbind(roc@alpha.values[[1]], roc@x.values[[1]], roc@y.values[[1]]))
colnames(perf) <-c("Probability","TNR","TPR")
perf <-perf[-1,]

library(reshape)
perf2<- melt(perf, measure.vars = c("TNR", "TPR"))

library(ggplot2)
g<-ggplot(perf2, aes(Probability, value, colour = variable)) + geom_line()+ theme_bw()

g+geom_hline(yintercept = 0.8)

plot(perf2$Probability, perf2$value)
abline(h=0.8)

f2 <- approxfun(perf2$value, perf2$Probability)
v0 <- 0.8
f2(v0)

library(SDMTools)
confusion.matrix (testset_2015$WD, pred_prob, threshold =0.170952)


###############################################################################


################################################################################
a<-humana4[,c("ADMISSIONS","READMISSIONS","AGE","SEX_CD","MONTHS_2016","RECON_MA_RISK_SCORE_NBR","RECON_RX_RISK_SCORE_NBR","Length_residence","Est_BMI_decile")]
summary(lm(a$READMISSIONS~.,data = a))

install.packages("randomForest")
require(randomForest)
fit=randomForest(factor(a$READMISSIONS)~., data=a)
impvar<-(varImp(fit, scale = FALSE))
plot(impvar)
barplot(t(impvar/sum(impvar)))

### Conditions ###
condition<-humana1[,c(107:362)]
as.data.frame(sort(table(rowSums (condition, na.rm = FALSE, dims = 1))))

### POT ###
pot<-z[,c(363:652)]
as.data.frame(sort(table(rowSums (pot, na.rm = FALSE, dims = 1))))

### RX ####
rx<-z[,c(653:921)]
as.data.frame(sort(table(rowSums (rx, na.rm = FALSE, dims = 1))))


# Overall
# ADMISSIONS              1139.71608
# AGE                      263.25256
# SEX_CD                    42.82261
# MONTHS_2016              116.75642
# RECON_MA_RISK_SCORE_NBR  350.21433
# RECON_RX_RISK_SCORE_NBR  320.83326
# Length_residence         233.65443
# Est_BMI_decile           171.49655


summary(lm(a$READMISSIONS~.,data=humana4))

# Total  8548 Null Values
write.csv(humana3,"D:/UTD/Competitions/humana_cleaned_data.csv")
# Removed the Null Values
humana<-humana[complete.cases(humana), ]

nums <- sapply(humana, is.numeric)
num_cols<-humana[ , nums]

nums <- sapply(humana, is.factor)
other_cols<-humana[ , nums]
names(other_cols)

# Removed the Null Values
num_cols1<-num_cols[complete.cases(num_cols), ]

# Removed the columns having Standard Devation zeros
j=0
for(i in seq(1:927))
{
    if(sd(num_cols1[,i])==0)
    {
      print(i)
      print(colnames(num_cols1)[i])
    }
}
humana1<-num_cols1[,-c(32,34,36,41, 59, 63,68,83,84,89,350,572,573,574,575,576,596,597,598,601,604,607,614,616,617,618,620,627,630,631,634, 637,638,640,641)]
names(humana3)

############# STEP WISE #############################

lmMod <- lm(humana3$READMISSIONS ~ . , data = humana3)
selectedMod <- step(lmMod)
summary(selectedMod)

all_vifs <- car::vif(selectedMod)
print(all_vifs)

signif_all <- names(all_vifs)

# Remove vars with VIF> 4 and re-build model until none of VIFs don't exceed 4.
while(any(all_vifs > 4)){
  var_with_max_vif <- names(which(all_vifs == max(all_vifs)))  # get the var with max vif
  signif_all <- signif_all[!(signif_all) %in% var_with_max_vif]  # remove
  myForm <- as.formula(paste("ozone_reading ~ ", paste (signif_all, collapse=" + "), sep=""))  # new formula
  selectedMod <- lm(myForm, data=inputData)  # re-build model with new formula
  all_vifs <- car::vif(selectedMod)
}
summary(selectedMod)

car::vif(selectedMod)

#################################


#################################
#install.packages("rpart")
#install.packages("e1071")
# install.packages("rpart.plot")
# install.packages("RColorBrewer")
# install.packages("randomForest")

library(caret)
library(rpart)
library(e1071)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

# Reading file from source
# Removing columns which contains NAs and Null Values
training1<-training1[,-c(12:36,50:59,69:83,87:101,103:112,125:139,141:150)]
testing1<-testing1[,-c(12:36,50:59,69:83,87:101,103:112,125:139,141:150)]

# removed column "new_window" and "X"
training1<-training1[,-c(1,6)]
# creating folds of training data for prediction 
inTrain <- createDataPartition(y=training1$classe, p=0.6, list=FALSE)
myTraining1 <- training1[inTrain, ]
myTesting1 <- training1[-inTrain, ]

clean1 <- colnames(myTraining1)
# Remove "classe" column 
clean2 <- colnames(myTraining1[, -58])  
myTesting1 <- myTesting1[clean1]
testing1 <- testing1[clean2]


for (i in 1:length(testing1) ) {
  for(j in 1:length(myTraining1)) {
    if( length( grep(names(myTraining1[i]), names(testing1)[j]) ) ==1)  {
      class(testing1[j]) <- class(myTraining1[i])
    }      
  }      
}

testing1 <- rbind(myTraining1[2, -58] , testing1)
testing1 <- testing1[-1,]


######## Decision Tree #########

modFitA11 <- rpart(classe ~ ., data=myTraining1, method="class")
#fancyRpartPlot(modFitA1)
predictionsA11 <- predict(modFitA11, myTesting1, type = "class")
confusionMatrix(predictionsA11, myTesting1$classe)
predictionsA21 <- predict(modFitA11, testing1, type = "class")

######## randomForest #########

modFitB11 <- randomForest(classe ~. , data=myTraining1)
predictionsB11 <- predict(modFitB11, myTesting1, type = "class")
confusionMatrix(predictionsB11, myTesting1$classe)
predictionsB21 <- predict(modFitB11, testing1, type = "class")

######### SVM #########
SVM_model1 <- svm(classe ~. , data=myTraining1,probability = TRUE)
predictionsC11 <- predict(SVM_model1, myTesting1, type = "class")
confusionMatrix(predictionsC11, myTesting1$classe)
predictionsC21 <- predict(SVM_model1, testing1, type = "class")

-------------------------------------------------------------------
  ## Out Of sample Error For Decision Tree
  outOfSampleError.accuracy <- sum(predictionsA11 == myTesting1$classe)/length(predictionsA11)
outOfSampleError.accuracy

# out of sample error and percentage of out of sample error
outOfSampleError <- 1 - outOfSampleError.accuracy
outOfSampleError

e <- outOfSampleError * 100
paste0("Out of sample error estimation: ", round(e, digits = 2), "%")
-------------------------------------------------------------------
  ## Out Of sample Error For Random Forest
  outOfSampleError.accuracy <- sum(predictionsB11 == myTesting1$classe)/length(predictionsB11)
outOfSampleError.accuracy

# out of sample error and percentage of out of sample error
outOfSampleError <- 1 - outOfSampleError.accuracy
outOfSampleError

e <- outOfSampleError * 100
paste0("Out of sample error estimation: ", round(e, digits = 2), "%")
-------------------------------------------------------------------
  
  ## Out Of sample Error For SVM
  outOfSampleError.accuracy <- sum(predictionsC11 == myTesting1$classe)/length(predictionsC11)
outOfSampleError.accuracy

# out of sample error and percentage of out of sample error
outOfSampleError <- 1 - outOfSampleError.accuracy
outOfSampleError

e <- outOfSampleError * 100
paste0("Out of sample error estimation: ", round(e, digits = 2), "%")
#-------------------------------------------------------------------

library(e1071)
SVM_model1 <- svm(train$ADMISSIONS ~. , data=train,probability = TRUE)
predictionsC11 <- predict(SVM_model1, test, type = "class")
confusionMatrix(predictionsC11, test$ADMISSIONS)

#-------------
fit <- glm(train$ADMISSIONS~.,data=train,family=binomial())
summary(fit) # display results
pred_prob<-predict (fit, newdata=test, type="response")
confusionMatrix(pred_prob, test$ADMISSIONS)


library (ROCR)
pred <- prediction(pred_prob, test$ADMISSIONS)
performance(pred, 'auc')
roc <- performance (pred,"tpr","tnr")
plot (roc)
perf <-as.data.frame(cbind(roc@alpha.values[[1]], roc@x.values[[1]], roc@y.values[[1]]))
colnames(perf) <-c("Probability","TNR","TPR")
perf <-perf[-1,]

library(reshape)
perf2<- melt(perf, measure.vars = c("TNR", "TPR"))

library(ggplot2)
g<-ggplot(perf2, aes(Probability, value, colour = variable)) + geom_line()+ theme_bw()

g+geom_hline(yintercept = 0.64)

plot(perf2$Probability, perf2$value)
abline(h=0.64)

f2 <- approxfun(perf2$value, perf2$Probability)
v0 <- 0.64
f2(v0)

library(SDMTools)
confusion.matrix (test$ADMISSIONS, pred_prob, threshold =0.1749318)
# obs
# pred    0    1
# 0    5695  723
# 1    3202 1315

length(pred_prob)
pred_prob
pred_prob<-as.data.frame(pred_prob)
pred_prob$Probability[pred_prob$pred_prob>0.1749318]<-1
pred_prob$Probability[pred_prob$pred_prob<=0.1749318]<-0
confusionMatrix(pred_prob$Probability, test$ADMISSIONS)
#############################################################






