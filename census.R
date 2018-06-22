adult <- read.table("adult.data.txt", stringsAsFactors = F)
na.omit(adult)
sapply(adult, function(x) sum(is.na(x)))

colnames(adult) <-c("Age","Class", "Fnlwgt", "Work", "Educ", "Marital", "Job", "Relationship", 
                    "Race", "sex", "Gain", "Loss", "Hours", "Country", "Over")
str(adult)
adult <-adult[complete.cases(adult),]
adult$Age<-as.numeric(gsub(",", "", adult$Age))
adult$Age <-as.numeric(adult$Age)

adult$Class <-as.factor(gsub(",", "", adult$Class))

adult$Fnlwgt <-as.numeric(gsub(",", "", adult$Fnlwgt))

adult$Work <-as.factor(gsub(",", "", adult$Work))

adult$Educ <-as.factor(gsub(",", "", adult$Educ))

adult$Marital <-as.factor(gsub(",", "", adult$Marital))

adult$Job <-as.factor(gsub(",", "", adult$Job))

adult$Relationship <-as.factor(gsub(",", "", adult$Relationship))

adult$Race <- as.factor(gsub(",", "", adult$Race))

adult$sex <- as.factor(gsub(",", "", adult$sex))

adult$Gain <- as.numeric(gsub(",", "", adult$Gain))

adult$Loss <- as.numeric(gsub(",", "", adult$Loss))

adult$Hours <-as.numeric(gsub(",", "", adult$Hours))

adult$Country <-as.character(gsub(",", "", adult$Country))
str(adult)

adult$Over <- ifelse(adult$Over==">50K", 1, 0)

adult$Over <-as.factor(adult$Over)
adult$Country <-as.factor(adult$Country)

library(ggplot2)
ggplot(adult, aes(x=Age, y=Hours))+geom_point()+
  geom_smooth(method = lm)
###No linear relationship

ggplot(adult, aes(x=Age, y=Gain)) + geom_point() 
###No linear relationship

ggplot(adult, aes(x=Age, y=Loss)) + geom_point() + 
  geom_smooth(method=lm)
###No linear relationship

ggplot(adult, aes(x=Hours, y=Loss)) + geom_point() +
  geom_smooth()
###No linear relationship

ggplot(adult, aes(x=Hours, y=Loss)) + geom_point() +
  geom_smooth()
###No linear relationship

ggplot(adult, aes(x=Gain, y=Loss)) + geom_point()
###No linear relationship

prop.table(table(adult$Over))
barplot(prop.table(table(adult$Over)), col=rainbow(2), 
        ylim= c(0, 0.7), main="Class Distribution")

library(caret)
trainIndex <-createDataPartition(adult$Over, p=0.70, list=FALSE )
adult_train <-adult[trainIndex, ]
adult_test <-adult[-trainIndex, ]

table(adult_train$Over)
prop.table(table(adult_train$Over))

###Predictive Model Random Forest 
library(randomForest)
rftrain <-randomForest(Over~., data=adult_train)

###Predictive Evaluation with test data 
library(caret)
confusionMatrix(predict(rftrain, adult_test), adult_test$Over,
                positive='1')
###Oversampling for better sensitivity
###Randomly Oversampling Examples
library(ROSE)
over <-ovun.sample(Over~., data=adult_train,method="over", N=34608)$data
table(over$Over)

rfover <-randomForest(Over~., data=over)
confusionMatrix(predict(rfover, adult_test), adult_test$Over, 
                positive='1')
###Accuracy went down and Sensitivity also went down

under<-ovun.sample(Over~., data = adult_train, method="under", N=10978)$data
table(under$Over)
rfunder <-randomForest(Over~., data=under)
confusionMatrix(predict(rfunder, adult_test), adult_test$Over, 
                positive='1')
###Sensitivity goes up, here!

###Both Oversampling and undersampling 

both <-ovun.sample(Over~., data=adult_train, method = "both", N=22793)$data
table(both$Over)
rfboth <-randomForest(Over~., data=both)
confusionMatrix(predict(rfboth, adult_test), adult_test$Over, 
                positive='1')
###Sensitivity slightly up here with both, but lower than undersampling

rose <-ROSE(Over~., data=adult_train, N=25000, seed=111)$data
table(rose$Over)
rfrose <-randomForest(Over~., data=rose)
confusionMatrix(predict(rfrose, adult_test), adult_test$Over, 
                positive='1')
###Accuracy slightly down, and sensitivity is down as well 
###The random Forest model predicts income below 50K very well, and above 50K not very well. 

##Linear Discriminant Analysis

library(MASS)
na.omit(adult)
fit <-lda(Over~Age+Class+Fnlwgt+Work+Educ+Marital+Job+
            Relationship+Race+sex+Gain+Loss+Hours+Country, 
          data=adult)
fit

pred <- predict(fit) #list of predicted 
head(pred)

class<-pred$class #The estimated class
head(class)

table(adult$Over, class)

correct <-mean(adult$Over==class)
correct
###84% Accuracy

plot(fit, main="Linear Discriminant Analysis")

###Validate LDA

fit <-lda(Over~Age+Class+Fnlwgt+Work+Educ+Marital+Job+
            Relationship+Race+sex+Gain+Loss+Hours+Country, 
          data=adult_train)
fit
#Predict on the test set 
pred_test <- predict(fit, newdata=adult_test)

#list of predicted values
class <-pred_test$class
head(class)

correct <-mean(adult_test$Over==class)
correct
plot(fit)

##Naive Bayes 

library(e1071)

bayes <-naiveBayes(Over~Age+Class+Fnlwgt+Work+Educ+Marital+Job+
            Relationship+Race+sex+Gain+Loss+Hours+Country, 
          data=adult_train)


pred <-predict(bayes, adult_test)
head(pred)

table(adult_test$Over, pred)

correct <- mean(adult_test$Over==class)
correct

confusionMatrix(predict(bayes, adult_test), adult_test$Over, 
                positive='1')



### I'll try an oversampled model

over <-ovun.sample(Over~., data=adult_train,method="over", N=34608)$data
table(over$Over)

nbover <-naiveBayes(Over~Age+Class+Fnlwgt+Work+Educ+Marital+Job+
                      Relationship+Race+sex+Gain+Loss+Hours+Country, 
                    data=over)
confusionMatrix(predict(nbover, adult_test), adult_test$Over, 
                positive='1')
###Accuracy went down a bit and sensitivity increased. 

under<-ovun.sample(Over~., data = adult_train, method="under", N=10978)$data
table(under$Over)
nbunder <-naiveBayes(Over~Age+Class+Fnlwgt+Work+Educ+Marital+Job+
                       Relationship+Race+sex+Gain+Loss+Hours+Country, 
                     data=under)
confusionMatrix(predict(nbunder, adult_test), adult_test$Over, 
                positive='1')
###Sensitivity goes up more, slightly 

both <-ovun.sample(Over~., data=adult_train, method = "both", N=22793)$data
table(both$Over)
nbboth <-naiveBayes(Over~Age+Class+Fnlwgt+Work+Educ+Marital+Job+
                      Relationship+Race+sex+Gain+Loss+Hours+Country, 
                    data=under)
confusionMatrix(predict(nbboth, adult_test), adult_test$Over, 
                positive='1')
###"Both" works the same as "under"

