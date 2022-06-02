library(readr)
bank<- read_delim("C:/Users/Tia Phan/OneDrive/Máy tính/a/Dataset/Bank Marketing/bank-additional-full.csv", 
                                             delim = ";", escape_double = FALSE, trim_ws = TRUE)
bank1=na.omit(bank)
distinct(bank1, y)
glimpse(bank)
table(bank$month)
View(bank)
str(bank1)
library(mlr3)
library(mlr3verse)

## Machine Learning
# Transform binary data into 1="yes" 0="no"
bank1$y<-ifelse(bank1$y=="yes",1,0)
bank1
# Count 1 and 0 in y column
table(bank1$y)
# Change data to numeric
bank1$y=as.numeric(bank1$y)
class(bank1$y)

# Splitting the data into training and testing datasets
set.seed(4000)
# Change all the character into factor
library(dplyr)
data = bank1 %>% mutate_if(sapply(bank1, is.character), as.factor)
str(data)
distinct(data, y)

# Set task and learner
tsk = tsk = as_task_regr(data, target = "y")
lrn = lrn("regr.rpart")
# Set train_set, test_set

train_set <- sample(tsk$nrow, 0.8 * tsk$nrow)
test_set <- setdiff(seq_len(tsk$nrow), train_set)
tsk_nozip = task_train$clone()$select(setdiff(tsk$feature_names, "y"))
task_train=tsk$clone()$filter(train_set)
lrn$train(tsk_nozip, row_ids = train_set)
lrn$model
predictions = lrn$predict(tsk, row_ids = test_set)
predictions
predictions$score
p=lrn$predict(tsk)
tab=as.data.table(p)
as_prediction_regr(tab)
tabs=split(tab,cut(tab$truth,3))
preds=lapply(tabs,as_prediction_regr)
sapply(preds,function(p) p$score)
# Linear
lm.fit <- glm(y~., data=train_set)
summary(lm.fit)

## Data Mining With R
install.packages("DMwR2")
library(DMwR2)
dm=sample(1:nrow(bank1),as.integer(0.7*nrow(bank1)))
trainDm=bank1[dm,]
testDm=bank1[-dm,]
nn3=kNN(y~.,trainDm,testDm,cl=bank1$y,k=3)
class(bank1$y)
bank1$y=as.numeric(bank1$y)
library(C50)
library(tree)
ind <- sample(2,nrow(bank1),replace=TRUE,prob=c(0.7,0.3))
traindata <- bank1[ind==1,]
testdata <-  bank1[ind==2,]
ftree=tree(y~.,data=traindata)
testdata
table(pred,testdata$y)
pred <- predict(ftree, testdata)
summary(pred)
confusionMatrix(factor(pred, levels = 1:5),factor(testdata$y, levels = 1:5))
confusionMatrix
print(paste())
bank[,c(14,15,16,5,8,9)]
names(bank1)
table(bank1$month)
bank1(7761)
