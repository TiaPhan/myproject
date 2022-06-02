house.data=read.csv("kc_house_data.csv",header = TRUE)
View(house.data)
dim(house.data)
library(ggplot2)
library(ggmap)
library(dplyr)
library(tidyverse)
library(caret)
library(corrplot) #Pearson's Corr
library(MASS) #Box-cox
library(car) #Vif
library(glmnet) #LASSO, Ridge
library(xgboost)
library(lubridate)
library(leaflet) #creates an interactive map
library(GGally) 
library(cluster)
library(caTools)
library(pacman)
p_load('tidyverse','rpart','rpart.plot','Metrics','forecast','caret',
       'ggplot2', 'FNN', 'fastDummies','dataPreparation','reshape2','corrplot')

# Change 20141013 to 2014-10-13
library("anytime")
house.data$date=anytime::anydate(house.data$date)

# Add column month
house.data$month<- strftime(house.data$date, "%m")
head(house.data$month)

min(house.data$date)
max(house.data$date)
# Check for null
apply(house.data,2,function(x) sum(is.na(x)))

# Round values down to the nearest integer:
house.data$bathrooms=floor(house.data$bathrooms)
house.data$floors=floor(house.data$floors)
table(house.data$floors)
# Count number of bedrooms
table(house.data$bedrooms)
house.data[house.data$bedrooms==33,]
house.data[-house.data$bedrooms==11,]
# Change 33 bedrooms to 3 bedrooms
house.data[house.data$bedrooms==33,]=house.data[house.data$bedrooms==3,]
warning()
# Heatmap 
library(corrplot)
data <- house.data %>% dplyr::select(-c(date))
corrplot(cor(data), method="number", type="lower")

# Add column "age"
house.data$age= 2015 - house.data$yr_built + 1
head(house.data$age)
 
# Hasn't renovated=1, Renovated=2
house.data$renovated= cut(house.data$yr_renovated, breaks = c(-1,0,3000), labels=c("1","2"))
house.data$renovated=as.numeric(house.data$renovated)
head(house.data$renovated)
# Different way to convert data to binary
df <- df %>% mutate(renovated = if_else(yr_renovated>0,1,0))
# Price type 1-4
house.data$price_cat = cut(house.data$price, breaks = c(0,350000,450000,700000,10000000), labels=c("1","2","3","4"))
head(house.data$price_cat)

# Select price, lat, long
coordinates_data = dplyr::select(house.data, price, lat, long)
head(coordinates_data)

# Mapping
library(leaflet)
pal = colorNumeric("YlOrRd", domain = coordinates_data$price)
int_map <- coordinates_data %>%
  leaflet()%>%
  addProviderTiles(providers$OpenStreetMap.Mapnik)%>%
  addCircleMarkers(col = ~pal(price), opacity = 1.1, radius = 0.3) %>% 
  addLegend(pal = pal, values = ~price) 
int_map

# Another way to map
library(ggmap)
qmplot(long, lat, maptype = "watercolor", color = log(price),
       data = house.data[train.idx[1:3000],]) +
  scale_colour_viridis_c()
qmplot(long, lat, maptype = "watercolor", color = zipcode,
       data = house.data[train.idx[1:3000],]) + guides(color = FALSE)

# Plot data 
plot(house.data$sqft_above, house.data$price, pch=19,col="pink", xlab='Square foot above',ylab='House Price', main = "Square footage of house apart from basement [br] vs Price")
ggplot(house.data, aes(x=grade, y=price))+geom_smooth()+labs(title="Grade vs Price")
ggplot(house.data, aes(x= sqft_living, y=price))+geom_point(col= house.data$grade)+labs(title="Living area vs Price")
distinct(house.data,grade)
boxplot(price~month,
        data=house.data,
        main="Different prices per month",
        xlab="Month",
        ylab="Price",
        col="orange",
        border="brown"
)
table(house.data$grade)
house.data[house.data$bathrooms==7,]

# Plot percent
library(ggplot2)
library(dplyr)
floors=house.data%>% 
  dplyr::count(floors)%>%
  dplyr::mutate(perc=n/sum(n)*100)
ggplot(data=floors,aes(x=floors, y=n))+geom_col(col="pink", fill="pink")+geom_text(aes(x=floors,y=n,label=paste0(n,"(",round(perc,1),"%)")))+theme_classic()

plot(price~bedrooms,
     data=house.data)
# Prediction Linear
set.seed(123)
split2=sample.split(house.data$price, SplitRatio = 0.8)
training_set2=subset(house.data, split2==TRUE)
test_set2=subset(house.data, split2==FALSE)
regressor1=lm(formula=price~., data=training_set2)
summary(regressor1)

# Delete insignificant variables. got regression 2
regressor2=lm(formula=price~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+long+sqft_lot15, data=training_set2)
summary(regressor2)

# Delete insignificant variables from regression 2, got regression 3
regressor3=lm(formula=price~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade+sqft_above+yr_built+yr_renovated+zipcode+lat+long, data=training_set2)
summary(regressor3)

regressor4=lm(formula=price~bedrooms+bathrooms+sqft_living+waterfront+view+condition+grade+sqft_above+zipcode+lat+long, data=training_set2)
summary(regressor4)

##Accuracy of the model on the train dataset
pred=regressor2$fitted.values
tally_table=data.frame(actual=training_set2$price, predicted=pred)
mape=mean(abs(tally_table$actual-tally_table$predicted)/tally_table$actual)
accuracy=1-mape
cat("The accuracy on the train data is:",accuracy)

pred_test2=predict(newdata=test_set2,regressor2)
tally_table=data.frame(actual=test_set2$price, predicted=pred_test2)
mape=mean(abs(tally_table$actual-tally_table$predicted)/tally_table$actual)
accuracy=1-mape
cat(" and the accuracy on the test data is:",accuracy)

# Calculate the MSE
prednew=predict(regressor2,newdata = test_set2)
cor(prednew,test_set2$price)
residuals2=(prednew-test_set2$price)
mse2=mean(residuals2)
mse2
sqrt(mse2)
# Create regression decision tree model
dt = rpart(price~.-id, data=training_set2, method='anova', cp= 0.01, minsplit = 30, xval = 10)
plot(dt)
text(dt)

# Decision tree
library(tree)
set.seed(1234)
train=sample(1:nrow(house.data),nrow(house.data)/2)
tree.house=tree(price~grade+view+sqft_living+yr_built,house.data,subset=train)
summary(tree.house)
plot(tree.house)
text(tree.house)
tree.house

yhat=predict(tree.house,newdata = house.data[-train,])
house.test=house.data[-train,"price"]
mse=mean((yhat-house.test)^2)
mse
sqrt(mse)

# Prune the tree
cv.king <- cv.tree(tree.house)
plot(cv.king$size, cv.king$dev, type = "b")


# Make var names available in script
attach(house.data)

# 1.Data Exploration
# View the structure of data
glimpse(house.data)
# View the summary of the data
summary(house.data)
# Make prices in 100k's
pricesIn100k=house.data$price/100000
#Price Distribution
hist(pricesIn100k,
     data=house.data,
     main = 'Distribution of Price',
     xlab = 'Price in $100k',
     ylab = 'Frequency',
     col = 'Blue',
     bins=10
     )

hist(house.data$mont,
     main="Number of house sales per month",
     xlab="Month",
     ylab="Number of sales",
     col="blue",
     type="highest")

# Bedroom Distribution
hist(as.numeric(priceByDecade$Decade),
     main='Distribution of Bedrooms',
     xlab = 'Number of Bedrooms',
     ylab = 'Frquency',
     col = 'green'
     )
class(house.data$month)
# Condition Distribution
hist(as.numeric(house.data$month),
     main = 'Frequency of House Sale Per Month',
     xlab = 'Month',
     ylab = 'Frequency',
     col = 'yellow'
     )
squareFt=house.data$sqft_lot15/100000
# Price by sqf

# Price by bedrooms
hist(house.data$bedrooms,
     main = 'Price By Bedrooms',
     xlab = 'Bedrooms',
     ylab = 'Frequency',
     col = 'purple'
     )
table(house.data$bedrooms)

# 2.MLR Model
# Create a baseline model
colnames(house.data)
house.model=lm(price ~.,data= house.data)
house.model1=lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+ waterfront+view + condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated+zipcode+lat+long+sqft_living15+sqft_lot15,data=house.data)
summary(house.model)
coeffs=summary(house.model)$coefficients
coeffs=round(coeffs, 4)
coeffs
plot(price~yr_built,
     data = priceByDecade,
     cex=.2,
     col='red',
     main='Price By Year',
     xlab='Year',
     ylab = 'Price of House in $100k'
     )
priceByDecade=data.frame(Price=house.data$price,Decade=house.data$yr_built)
min(priceByDecade$Decade)
max(priceByDecade$Decade)
for (i in 1:5000) {
  if(priceByDecade$Decade[i]<1925){
    priceByDecade$Decade[i]='1900-1925'
  }
  else if(priceByDecade$Decade[i]>1925 && priceByDecade$Decade[i]<1950){
    priceByDecade$Decade[i]='1925-1950'
  }
  else if(priceByDecade$Decade[i]>1950 && priceByDecade$Decade[i]<1975){
    priceByDecade$Decade[i]='1950-1975'
  }
  else if(priceByDecade$Decade[i]>1975 && priceByDecade$Decade[i]<2000){
    priceByDecade$Decade[i]='1975-2000'
  }
  else{
    priceByDecade$Decade[i]='2000-current'
  }
}
priceByDecade$Decade=as.factor(priceByDecade$Decade)
anova=aov(Price~Decade, data = priceByDecade)
summary(anova)
anova1=aov(price~.,data=house.data)
summary(anova1)
TukeyHSD(anova)
plot(house.data$price~Decade,
     data = priceByDecade,
     main='ANOVA Price~Qtr.Century',
     xlab='Qrt.Century 1900-Present',
     ylab='Price In $100k',
     col=c('orange','blue','yellow','pink','green')
)
summary(house.data$price)

library(mlr3verse)
ggplot(house.data, aes(x = price)) + geom_density()
house.data$price

autoplot(tsk)+facet_wrap(~condition)
dim(house.data)
names(house.data)
house.data$id = NULL

tsk = as_task_regr(house.data, target = "price")
lrn = lrn("regr.rpart")
train.idx = sample(seq_len(tsk$nrow), 0.7 * tsk$nrow)
test.idx = setdiff(seq_len(tsk$nrow), train.idx)
task_train = tsk$clone()$filter(train.idx) 
task_test  = tsk$clone()$filter(test.idx)
tsk_nozip = task_train$clone()$select(setdiff(tsk$feature_names, "zipcode"))

lrn$train(tsk_nozip, row_ids = train.idx)
lrn$model
plot(lrn$model)
text(lrn$model)
prediction <- lrn$predict(tsk, row_ids = task_test)
confusionMatrix(house.model, task_test)

house.data$yr_renovated
max(house.data$yr_renovated)
distinct(house.data, yr_renovated)
head(house.data)
distinct(house.data,lat)
library(ggplot2)
ggplot(house.data, aes(x=lat, y=price, color=waterfront)) +
  geom_point(size=1)

E)
