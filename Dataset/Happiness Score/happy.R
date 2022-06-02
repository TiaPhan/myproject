library(readr)
hp1 <- read_csv("C:/Users/Tia Phan/OneDrive/Máy tính/a/Dataset/Happiness Score/new-world-happiness-report-2021.csv")
View(hp1)



# Clean

hp1$`Ladder score in Dystopia`=NULL
hp1$`Explained by: Log GDP per capita`=NULL
hp1$`Explained by: Social support`=NULL
hp1$`Explained by: Healthy life expectancy`=NULL
hp1$`Explained by: Freedom to make life choices`=NULL
hp1$`Explained by: Generosity`=NULL
hp1$`Explained by: Perceptions of corruption`=NULL
hp1$`Standard error of ladder score`=NULL
hp1$upperwhisker=NULL
hp1$lowerwhisker=NULL
hp1$`Dystopia + residual`=NULL

# Plot

ggplot(data = hp1)+geom_point(mapping = aes(x = `Region`, y = `Ladder score`))
ggplot(data = hp1)+geom_point(mapping = aes(x=`Logged GDP per capita`, y=`Ladder score`), color="blue")+geom_smooth(data=hp1, aes(x=`Logged GDP per capita`, y=`Ladder score`, color="1"), formula = y~x,, method="lm") 
ggplot(data = hp1)+geom_point(mapping = aes(x=`Social support`, y=`Ladder score`), color="blue")+geom_smooth(data=hp1, aes(x=`Social support`, y=`Ladder score`, color="1"), formula = y~x,, method="lm") 
ggplot(data = hp1)+geom_point(mapping = aes(x=`Healthy life expectancy`, y=`Ladder score`), color="blue")+geom_smooth(data=hp1, aes(x=`Healthy life expectancy`, y=`Ladder score`, color="1"), formula = y~x,, method="lm") 
ggplot(data = hp1)+geom_point(mapping = aes(x=`Freedom to make life choices`, y=`Ladder score`), color="blue")+geom_smooth(data=hp1, aes(x=`Freedom to make life choices`, y=`Ladder score`, color="1"), formula = y~x,, method="lm")
ggplot(data = hp1)+geom_point(mapping = aes(x=`Generosity`, y=`Ladder score`), color="blue")+geom_smooth(data=hp1, aes(x=`Generosity`, y=`Ladder score`, color="1"), formula = y~x,, method="lm")
ggplot(data = hp1)+geom_point(mapping = aes(x=`Perceptions of corruption`, y=`Ladder score`), color="blue")+geom_smooth(data=hp1, aes(x=`Perceptions of corruption`, y=`Ladder score`, color="1"), formula = y~x,, method="lm")

ggplot(na.omit(hp1),aes(`Ladder score`))+geom_bar(fill="pink")

summary(hp1[,3:12])                                
dim(hp1)                                
tail(hp1[,1:3])
min(hp1$`Ladder score`)

# Regression 

hp.model=lm(`Ladder score` ~`Logged GDP per capita` ,data= hp1)
summary(hp.model)
hp.lm <- lm(`Ladder score` ~`Logged GDP per capita`, data = hp1)
summary(hp.lm)
happy.lm<-lm(`Ladder score` ~`Logged GDP per capita`+`Perceptions of corruption`+`Generosity`+`Freedom to make life choices`+`Healthy life expectancy`+`Social support`, data = hp1)
summary(happy.lm)

# Arules


hp1[["Ladder score"]]=ordered(cut(hp1[["Ladder score"]],c(3.145,4,7.2)),labels=c("Unhappy","Happy"))
hp1[["Logged GDP per capita"]]=ordered(cut(hp1[["Logged GDP per capita"]],c(6.635,8,11.1)),labels=c("Low_GDP","High_GDP"))
summary(hp1$`Perceptions of corruption`)
hp1[["Perceptions of corruption"]]=ordered(cut(hp1[["Perceptions of corruption"]],c(0.16,0.7,1)),labels=c("Low_level_corruption","High_level_corruption"))
summary(hp1)
hp1[["Generosity"]]=ordered(cut(hp1[["Generosity"]],c(0,0.1,0.5)),labels=c("Stingy","Generous"))
hp1[["Freedom to make life choices"]]=ordered(cut(hp1[["Freedom to make life choices"]],c(0,0.4,0.7)),labels=c("No_freedom","Free"))
hp1[["Healthy life expectancy"]]=ordered(cut(hp1[["Healthy life expectancy"]],c(0.01,0.2,1)),labels=c("Low_health_expect","Healthy"))
hp1[["Healthy life expectancy"]]=ordered(cut(hp1[["Healthy life expectancy"]],c(0.05,0.6,1.8)),labels=c("Low_health_expect","Healthy"))
rules=apriori(hp1,parameter = list(supp=0.01,conf=0.6))
hp=transactions(hp1)

unhappy=subset(rules,subset=rhs%in% "Ladder score=Unhappy"&lift>1)
inspect(unhappy[1:5])
happy=subset(rules,(rhs %in% "Ladder score=Happy" & lift>1.2))
inspect(tail(happy,n=5))

rule <- apriori(hp1, 
                parameter=list(minlen=2, supp=0.1, conf=0.7),  
                appearance = list(default="lhs",
                                  rhs=c("Ladder score=Unhappy", "Ladder score=Happy") 
                )
)
inspect(head(rule, n=3))
summary(hp1)
hp1[["Freedom to make life choice"]]=ordered(cut(hp1[["Freedom to make life choice"]], c(0.4,0.7,1)), labels=c("No_freedom","Free"))
class(hp1$`Freedom to make life choices`)

# Correlation
plot(hp1, main="Correlation")
cor(hp1[, unlist(lapply(hp1, is.numeric))])
# Correlation Heatmap
install.packages("metan")
library(metan)
corrl=corr_coef(hp1[,-1:-2])
plot(corrl)

na.omit(hp1[hp1$`Ladder score`=="Unhappy",])
with(hp1[hp1$`Ladder score`=="Unhappy",], plot(`Country name`, `Ladder score`, main="Unhappy Countries", xlab="Country name", ylab="Ladder score"))
hp1$`Ladder score`=as.numeric(hp1$`Ladder score`)
class(hp1$`Ladder score`)
hp1$`Country name`=as.numeric(hp1$`Country name`)
class(hp1$`Country name`)
heatmap(cor(hp1, is.numeric))
