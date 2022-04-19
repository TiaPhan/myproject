data<- read.csv("C:/Users/Tia Phan/Downloads/COVID19_line_list_data.csv")
install.packages("Hmisc")
library(Hmisc)
describe(data)#descriptive statistic
data$death_dummy=as.integer(data$death != 0)# cleaned up death column
# claim: people who die are older
dead=subset(data,death_dummy==1)
alive=subset(data,death_dummy==0)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm=TRUE)
t.test(dead$age,alive$age,alternative="two.sided",conf.level=0.5)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant
male=subset(data,gender=="male")
female=subset(data,gender=="female")
mean(female$death_dummy)
mean(male$death_dummy)
t.test(male$death_dummy,female$death_dummy,alternative = "two.sided",conf.level = 0.95)
# 99% confidence: men have from 0.8% to 8.8% higher chance
# of dying.
# p-value = 0.002 < 0.05, so this is statistically
# significant