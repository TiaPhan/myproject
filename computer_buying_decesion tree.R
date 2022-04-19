##https://www.guru99.com/r-decision-trees.html
library(C50)
computer=read.csv("da_computer3.txt")
attach(computer)
?attach  #attach your database to R
?na.omit #removed all NAs
ff=na.omit(computer)
ftree=tree(age~., data=ff);ftree
names(ff)
prop.table(table(ff$buy)) #buying percentage in ff dataset
install.packages("tree")
library(tree)
?trees
library(dplyr)
library(tidyr)
ff=ff%>%separate(edu.age.marri.income.city.buy, c("edu","age","marri","income","city","buy"))
names(ff)
ftree=tree(buy~.,data=ff);ftree
summary(ftree)
class(ftree)
head(ftree)
plot(ftree)
text(ftree)
show(ftree)
ind <- sample(2,nrow(ff),replace=TRUE,prob=c(0.7,0.3)) #The common practice is to split the data 80/20, 80 percent of the data serves to train the model, and 20 percent to make predictions.
?sample
traindata <- ff[ind==1,]
traindata
testdata = ff[ind==2,]
testdata
create_buy_test <- function(ff, size = 0.8, buy = TRUE) {
  n_row = nrow(ff)
  total_row = size * n_row
  buy_sample < - 1: total_row
  if (train == TRUE) {
    return (data[ff, ])
  } else {
    return (data[-ff, ])
  }
}
data_test <- create_buy_test(buy, 0.8, buy = FALSE)
prop.table(table(traindata$buy)) #buying percentage in traindata set
install.packages("rpart")    #regression plot
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(buy~., data = traindata, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, testdata, type = 'class')  #predict which passengers are more likely to survive after the collision from the test set. It means, you will know among those 209 passengers, which one will buy or not.
table_mat <- table(testdata$buy, predict_unseen)
