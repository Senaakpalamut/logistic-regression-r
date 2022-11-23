#LOGISTIC REGRESSION IMPLEMENTATION BY MTCARS DATASET
install.packages('dplyr')
library(dplyr)
summary(mtcars)

#performing the Logistic Regression by using glm() function.
install.packages('caTools') #for logistic regression
install.packages('ROCR') #for roc, roc-auc curves
library(caTools)
library(ROCR)

split<-sample.split(mtcars, SplitRatio=0.8)
train_reg<-subset(mtcars, split='TRUE')
test_reg<-subset(mtcars, split='FALSE')

logistic_model<-glm(vs~ wt +disp, data=train_reg, family='binomial')
logistic_model
summary(logistic_model)

#prediction
pred_reg<- predict(logistic_model, test_reg, type='response')
pred_reg
#changing the probabilities
pred_reg<-ifelse(pred_reg>0.5,1,0)


#evaluate the model accuracy by using confusion matrix
table(test_reg$vs, pred_reg)

missing_class<-mean(pred_reg != test_reg$vs)
print(paste('Accuracy = ', 1-missing_class))

#ROC / ROC-AUC Curve
ROCpred<- prediction(pred_reg, test_reg$vs)
ROCperf<-performance(ROCpred, measure='tpr', x.measure='fpr')

auc<- performance(ROCpred, measure = 'auc')
auc<- auc@y.values[[1]]
auc

#plot the curves
plot(ROCperf)
plot(ROCperf, colorize=TRUE, print.cutoffs.at=seq(0.1, by=0.1),
     main='ROC curve')

abline(a=0, b=1)
auc<-round(auc, 4)
legend(.6, .4, auc, title='auc', cex=1)
