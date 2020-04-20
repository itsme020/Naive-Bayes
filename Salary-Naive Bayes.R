


library(plyr)

summary(creditcard)

table(creditcard$card)

#Change Character to numeric
creditcard$owner <- as.factor(revalue(creditcard$owner,c("yes"=1,"no"=0)))
creditcard$selfemp <- as.factor(revalue(creditcard$selfemp,c("yes"=1,"no"=0)))
attach(creditcard)


sum(is.na(creditcard))
dim(creditcard)
colnames(creditcard)

# Preparing a linear regression 
LM1<- lm(card~.,data = creditcard)
pred1 <- predict(LM1,creditcard)
plot(creditcard$card,pred1)
plot(pred1)

# The output of sigmoid function lies in between 0-1
model <- glm(card~.,data=creditcard,family = "binomial")

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))
# Confusion matrix table 
prob <- predict(model,creditcard,type="response")
prob
summary(model)

confusion<-table(prob>0.5,creditcard$card)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy##### 0.877

# Creating empty vectors based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
creditcard[,"prob"] <- prob
creditcard[,"pred_values"] <- pred_values
creditcard[,"yes_no"] <- yes_no

# View table

table(creditcard$card,creditcard$pred_values)

##ROC Curve
install.packages("ROCR")
library(ROCR)
predterm<-prediction(prob,creditcard$card)
rocrpredterm<-performance(predterm,'tpr','fpr')
plot(rocrpredterm)


###getting treshhold values

rocr.cutoff <- data.frame(cut_off = rocrpredterm@alpha.values[[1]],fpr=rocrpredterm@x.values,tpr=rocrpredterm@y.values)
colnames(rocr.cutoff) <- c("cut_off","FPR","TPR")
View(rocr.cutoff)

library(dplyr)
rocr.cutoff$cut_off <- round(rocr.cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr.cutoff <- arrange(rocr.cutoff,desc(TPR))
View(rocr.cutoff)



