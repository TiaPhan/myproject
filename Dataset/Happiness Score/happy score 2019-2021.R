library(readxl)
happiness_1921 <- read_excel("C:/Users/Tia Phan/OneDrive/Máy tính/a/Dataset/Happiness Score/happiness 1921_1.xlsx")
View(happiness_1921)
library(ggplot2)
library(dplyr)
library(arules)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(ggcorrplot)
library(choroplethr)
library(ggmap)
# Check for null
apply(hap,2,function(x) sum(is.na(x)))

# Top happiest countries by years
Y2019=happiness_1921[happiness_1921$Score==2019,]
#or
Y2019=happiness_1921[which(happiness_1921$Year==2019),]
Y2020=happiness_1921[happiness_1921$Year==2020,]
Y2021=happiness_1921[happiness_1921$Year==2021,]
top_6_2021=head(arrange(Y2021,desc(Score)))
# Plot
options(repr.plot.width=8, repr.plot.height=3)
ggplot(top_6_2019, aes(x = Score, y = Country, main="2020 Happiness")) +
  geom_bar(stat = "identity",fill="pink") +
  coord_flip() + scale_x_continuous(name="Average Happiness Score") +
  scale_y_discrete(name="Country") +
  theme(axis.text.x = element_text(face="bold", color="#008000",
                                   size=8, angle=0),
        axis.text.y = element_text(face="bold", color="#008000",
                                   size=8, angle=0))
# reshape your data into long format
library(reshape2)
nyxlong <- melt(happiness_1921, id=c("GDP_per_capita","Country"))
a1=dplyr::select(happiness_1921,Country,Year,Social_support)
a1
a2=a1[a1$Year==2021,]
a3=a2[a2$Country==c("Isreal","Zimbabwe"),]
a3
# make the plot
ggplot(nyxlong) +
  geom_bar(aes(x = Country, y =GDP_per_capita, fill = variable), 
           stat="identity", position = "dodge", width = 0.7) +
  scale_fill_manual("Result\n", values = c("red","blue"), 
                    labels = c(" Yresult", " Xresult")) +
  labs(x="\nNumber",y="Result\n") +
  theme_bw(base_size = 14)
# Correlation
corr_data <- happiness_1921 %>% select(Score, GDP_per_capita, Social_support, Health, Freedom, Generosity, Corruption)
corr <- round(cor(corr_data), 1)
corr
ggcorrplot(corr, ggtheme = ggplot2::theme_minimal(),
           colors = c("#FDEE87", "white", "#50C878"), lab=TRUE, title ="Overall Correlation") 


# Average
avg.GDP.2021=mean(Y2021$GDP_per_capita)
avg.GDP.2021
avg.social.support.2021=mean(Y2021$Social_support)
avg.social.support.2021
avg.health.2021=mean(Y2021$Health)
avg.feedom.2021=mean(Y2021$Freedom)
avg.feedom.2021
avg.generosity.2021=mean(Y2021$Generosity)
avg.generosity.2021
avg.corruption.2021=mean(Y2021$Corruption)
avg.corruption.2021
avg.score.2021=mean(Y2021$Score)
avg.score.2021
# Avg dataframe
avg=data.frame(
  Year = c(2019,2020,2021),
  Score=c(4.58,4.64,4.73),
  GDP = c(0.64,8.45,8.59),
  Social.support = c(1.01,0.72,0.73),
  Health=c(0.52,58.19,59),
  Freedom=c(0.32,0.71,0.72),
  Generosity=c(0.17,-0.02,-0.01),
  Corruption=c(0.09,0.77,0.76),
  stringsAsFactors = FALSE
)

# Exploring Trend by Plotting
ggplot(data=avg,aes(x=Year, y=Score))+geom_line(col="pink")

# Horizontal bar Plot
ggplot(data=top_6_2021, aes(x=Country, y=Score)) +
  geom_bar(stat="identity", fill="#FDEE87") + coord_flip() + theme_linedraw() + ggtitle("2021 Top 6 Happiest Countries") + xlab("Countries") + ylab("Ladder Score")
# Basic bar plot
options(repr.plot.width=10, repr.plot.height=3)
ggplot(data=happiness_1921,aes(x=Cou,y=Score))+geom_col(aes(fill=Year),width=0.8)


plot(avg$Year,avg$Generosity,type = "o", col = "red", xlab = "Year", ylab = "Score",
     main = "Average Generosity by year")
ggplot(happiness_1921,aes(x=Freedom,y=Score))+geom_point(col="pink")+geom_smooth(data=happiness_1921,aes(x=Freedom,y=Score,color="1"),formula = y~x, method="lm") 
boxplot(happiness_1921$Score~happiness_1921$Regional,col=rainbow(length(unique(happiness_1921$Regional))),cex.axis=0.5)

# Linear Regression
library(caTools)
set.seed(123)
split2=sample.split(happiness_1921$Score, SplitRatio = 0.8)
training_set2=subset(happiness_1921, split2==TRUE)
test_set2=subset(happiness_1921, split2==FALSE)
regressor3=lm(formula=Score~Social_support+Generosity+Corruption, data=training_set2)
summary(regressor3)

pred=regressor2$fitted.values
tally_table=data.frame(actual=training_set2$Score, predicted=pred)
mape=mean(abs(tally_table$actual-tally_table$predicted)/tally_table$actual)
accuracy=1-mape
cat("The accuracy on the train data is:",accuracy)

pred_test2=predict(newdata=test_set2,regressor2)
tally_table=data.frame(actual=test_set2$Score, predicted=pred_test2)
mape=mean(abs(tally_table$actual-tally_table$predicted)/tally_table$actual)
accuracy=1-mape
cat(" and the accuracy on the test data is:",accuracy)

prednew=predict(regressor1,newdata = test_set2)
cor(prednew,test_set2$Score)
residuals2=(prednew-test_set2$Score)
mse2=mean(residuals2)
mse2

#CLustering
hap2021 <- read_excel("C:/Users/Tia Phan/OneDrive/Máy tính/a/Dataset/Happiness Score/hap2021.xlsx")
View(hap2021)
library(ggplot2)
ggplot(happiness_1921,aes(Rank,Score,color=Regional))+geom_point()
set.seed(123)
happy_cluster=kmeans(happiness_1921[,6:12],centers = 10,iter.max = 100,nstart = 20)
happy_cluster
happy_cluster$size
data.with.cluster=data.frame(happiness_1921$Regional,happy_cluster$cluster)
data.with.cluster
summary(data.with.cluster)
# Box plot of cluster
boxplot(data.with.cluster$happy_cluster.cluster~happiness_1921$Regional,col=rainbow(length(unique(happiness_1921$Regional))),cex.axis=0.5)
# Structure
str(hap)
colnames(hap)
# Decision Tree
library(tree)
set.seed(1234)
train=sample(1:nrow(happiness_1921),nrow(happiness_1921)/2)
tree.house=tree(Score~GDP_per_capita+Social_support+Generosity+Corruption,happiness_1921,subset=train)
summary(tree.house)
plot(tree.house)
text(tree.house)

yhat=predict(happiness_1921,newdata = happiness_1921[-train,])
house.test=happiness_1921[-train,"Score"]
mse=mean((yhat-house.test)^2)
mse
# Compare 
israel = hap2021[which(hap2021$Country=="Israel"),]
zimbabwe=hap2021[which(hap2021$Country=="Zimbabwe"),]
israel$Score
zimbabwe$Score
view(hap2021)
year2019=happiness_1921[which(happiness_1921$Year==2019),]
avg
# Plot comparing map

ggplot(hap2021, aes(x=Score)) + 
  geom_histogram(binwidth=.2, fill="#50C878") + ggtitle("2021 Social Support") + xlab("Social support") + ylab("Country Count") + geom_vline(data=israel, aes(xintercept=Score, color="#FF7F24"),linetype="solid") + geom_vline(data=zimbabwe, aes(xintercept=Social_support, color="#CAFF70"),linetype="solid") +scale_color_discrete(name = "Legend", labels = c("israel", "zimbabwe")) + theme_linedraw()
ggplot(hap2021, aes(x=Corruption)) + 
  geom_histogram(binwidth=.025, fill="#50C878") + ggtitle("2021 Corruption") + xlab("Corruption") + ylab("Country Count") + geom_vline(data=israel, aes(xintercept=Corruption, color="#E13F2A"),linetype="solid") + geom_vline(data=zimbabwe, aes(xintercept=Corruption, color="#3FE0D0"),linetype="solid") +scale_color_discrete(name = "Legend", labels = c("Zimbabwe", "Israel")) +theme_linedraw() 


