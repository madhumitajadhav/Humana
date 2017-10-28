flags<-humana1[,c(31:96)]
flag_2014<-flags[ ,grepl("FLAG_",names(flags)) & grepl("^.+(2014)$",names(flags))]
flag_2015<-flags[ ,grepl("FLAG_",names(flags)) & grepl("^.+(2015)$",names(flags))]

names(flag_2015)
flag_2015<-flag_2015[,-c(9,11,13,17,18)]
flag_2015$FLAG_Diab_Complications_2015<-as.factor(flag_2015$FLAG_Diab_Complications_2015)

summary(glm(flag_2015$FLAG_Diab_Complications_2015~.,data=flag_2015,family=binomial()))
summary(lm(as.numeric(flag_2015$FLAG_Diab_Complications_2015)~.,data=flag_2015))

# Call:
#   glm(formula = flag_2015$FLAG_Diab_Complications_2015 ~ ., family = binomial(), 
#       data = flag_2015)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.6031  -0.9195  -0.8418   1.3019   1.7034  
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             -0.658373   0.033606 -19.591  < 2e-16 ***
#   FLAG_CV_CAD_2015         0.176990   0.027850   6.355 2.08e-10 ***
#   FLAG_CV_CIR_2015         0.146678   0.026816   5.470 4.51e-08 ***
#   FLAG_CV_HDZ_2015         0.009509   0.028099   0.338 0.735044    
# FLAG_CV_PVD_2015        17.761371  73.827966   0.241 0.809882    
# FLAG_RES_ALG_2015       -0.089253   0.039927  -2.235 0.025390 *  
#   FLAG_RES_AST_2015        0.055805   0.042835   1.303 0.192642    
# FLAG_RES_COPD_2015       0.049340   0.032259   1.529 0.126148    
# FLAG_RES_INF_2015       -0.038776   0.028679  -1.352 0.176349    
# FLAG_CANCER_2015        -0.048775   0.036699  -1.329 0.183830    
# FLAG_CANCER_ACTIVE_2015        NA         NA      NA       NA    
# FLAG_HYPERTENSION_2015  -0.033396   0.031257  -1.068 0.285325    
# FLAG_HYPERLIPID_2015    -0.163492   0.027328  -5.983 2.20e-09 ***
#   FLAG_RENAL_2015          1.040298   0.029052  35.808  < 2e-16 ***
#   FLAG_CKD_2015                  NA         NA      NA       NA    
# FLAG_ESRD_2015                 NA         NA      NA       NA    
# FLAG_DEPRESSION_2015     0.115490   0.030879   3.740 0.000184 ***
#   FLAG_MUSCUL_OTH_2015     0.073132   0.070906   1.031 0.302356    
# FLAG_MUSCUL_BN_2015      0.097573   0.026083   3.741 0.000183 ***
#   FLAG_OSTEO_2015         -0.130764   0.050934  -2.567 0.010248 *  
#   FLAG_ARTH_2015          -0.079275   0.028694  -2.763 0.005731 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 49852  on 36451  degrees of freedom
# Residual deviance: 43518  on 36434  degrees of freedom
# AIC: 43554
# 
# Number of Fisher Scoring iterations: 16


names(flag_2014)
flag_2014<-flag_2014[,-c(10,12,14,18,19)]
flag_2014$FLAG_Diab_Complications_2014<-as.factor(flag_2014$FLAG_Diab_Complications_2014)

summary(glm(flag_2014$FLAG_Diab_Complications_2014~.,data=flag_2014,family=binomial()))

summary(lm(as.numeric(flag_2014$FLAG_Diab_Complications_2014)~.,data=flag_2014))


############## Random Forest 2014 ####################

random_forest2014=randomForest(flag_2014$FLAG_Diab_Complications_2014~.,data=flag_2014,ntree=500,importance=TRUE)
random_forest2014

plot(random_forest2014,main="Random Forest: Error Rate vs Number of Trees")

imp=importance(random_forest2014)
impL=imp[,c(3,4)]
imp.ma=as.matrix(impL)
imp.df=data.frame(imp.ma)

write.csv(imp.df, "D:/UTD/Competitions/Humana/imp.df_2014.csv", row.names=TRUE)
imp.df.csv_2014=read.csv("D:/UTD/Competitions/Humana/imp.df_2014.csv",header=TRUE)

colnames(imp.df.csv_2014)=c("Variable","MeanDecreaseAccuracy","MeanDecreaseGini")
imp.sort =  imp.df.csv_2014[order(-imp.df.csv_2014$MeanDecreaseAccuracy),] 

imp.sort = transform(imp.df.csv_2014,Variable = reorder(Variable, MeanDecreaseAccuracy))

VIP=ggplot(data=imp.sort, aes(x=Variable, y=MeanDecreaseAccuracy)) + 
  ylab("Mean Decrease Accuracy")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.75)+ 
  coord_flip()+theme_few() 

imp.sort.Gini <- transform(imp.df.csv_2014, 
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

###############################################

random_forest2015=randomForest(flag_2015$FLAG_Diab_Complications_2015~.,data=flag_2015,ntree=500,importance=TRUE)
random_forest2015

plot(random_forest2015,main="Random Forest: Error Rate vs Number of Trees")

imp=importance(random_forest2015)
impL=imp[,c(3,4)]
imp.ma=as.matrix(impL)
imp.df=data.frame(imp.ma)

write.csv(imp.df, "D:/UTD/Competitions/Humana/imp.df_2015.csv", row.names=TRUE)
imp.df.csv_2015=read.csv("D:/UTD/Competitions/Humana/imp.df_2015.csv",header=TRUE)

colnames(imp.df.csv_2015)=c("Variable","MeanDecreaseAccuracy","MeanDecreaseGini")
imp.sort =  imp.df.csv_2015[order(-imp.df.csv_2015$MeanDecreaseAccuracy),] 

imp.sort = transform(imp.df.csv_2015,Variable = reorder(Variable, MeanDecreaseAccuracy))

VIP=ggplot(data=imp.sort, aes(x=Variable, y=MeanDecreaseAccuracy)) + 
  ylab("Mean Decrease Accuracy")+xlab("")+
  geom_bar(stat="identity",fill="skyblue",alpha=.8,width=.75)+ 
  coord_flip()+theme_few() 

imp.sort.Gini <- transform(imp.df.csv_2015, 
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




