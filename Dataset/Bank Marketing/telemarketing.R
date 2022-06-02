library(readr)
library(caret)
library(gridExtra)
library(GGally)
library(rsample)
library(e1071)
library(ROCR)
library(randomForest)
library(rpart)
library(rpart.plot)
bank <- read_delim("C:/Users/Tia Phan/OneDrive/Máy tính/a/Dataset/Bank Marketing/bank.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(bank)
glimpse(bank)
distinct(bank_clean_,month)
bank=na.omit(bank)

data = bank %>% mutate_if(sapply(bank, is.character), as.factor)
str(data)
head(bank_clean_)
transform <- model.matrix(y ~ ., data = bank) %>% 
  as.data.frame() %>% 
  select(c("defaultyes","housingyes","loanyes"))

# Transform data Yes=1, No=0
data$default=transform$defaultypes
data$housing=transform$housingyes
data$loan=transform$loanyes
head(bank)
prop.table(table(bank$y))

# Split train and test dataset
RNGkind(sample.kind = "Rounding")
set.seed(100)
index=initial_split(data=data, prop = 0.8,strata = "y")
bank_train=training(index)
bank_test=testing(index)

# Data pre-pocessing
cust_down_train <- downSample(x = bank_train %>% select(-y),
                              y = bank_train$y,
                              yname = "y")

prop.table(table(cust_down_train$y))

# Naive
model_naive <- naiveBayes(x = cust_down_train %>% select(-y),
                          y = cust_down_train$y,
                          laplace = 1 
)

# Predict Naive
pred_naive <- predict(object = model_naive,
                      newdata = bank_test,
                      type = "class")
confusionMatrix(data = pred_naive, 
                reference = bank_test$y, 
                positive = "yes")

# Decision Tree
library(partykit)
cust_tree <- ctree(formula = y ~ ., data = cust_down_train, control = ctree_control(mincriterion = 0.5,
                                                                                    minsplit = 500,
                                                                                    minbucket = 1200))
cust_tree
plot(cust_tree,type="simple")
predict_tree <- predict(object = cust_tree,
                        newdata = bank_test)

predict_prob_tree <- predict(cust_tree,
                             bank_test,
                             type ="prob")
confusionMatrix(predict_tree, bank_test$y, positive = "yes")


# Random Forest
n0_var <- nearZeroVar(cust_down_train)
down_cust <- cust_down_train[, -n0_var]
dim(down_cust)
set.seed(2018)
ctrl <- trainControl(method="repeatedcv", # k-fold cross validation
                     number=4, 
                     repeats=3) 
forest <- train(y ~ ., data=cust_down_train , 
                method="rf", 
                trControl = ctrl)
model_RF <- forest
model_RF

model_RF$finalModel
predict.forest <- predict(model_RF, bank_test, type = "raw")# for the class prediction
predict.prob.forest <- predict(model_RF, bank_test, type = "prob")# for the probability
confusionMatrix(predict.forest, bank_test$y, positive = "yes")
forest_roc <- predict(predictions = predict.prob.forest[,2], # = kelas positif
                         labels = as.numeric(customer_test$y == "yes"))
#plot
plot(forest_perf, main = "ROC")
abline(0,1, lty =2)
library(pROC)
roc(bank_test$y, predict_tree)
