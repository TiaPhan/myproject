library(arules)
library(arulesViz)
rtl = read.transactions("retail.dat",format = "basket")
rtl_size=size(rtl) #To get the actual number of items in the itemsets stored in the itemMatrix, size() is used.
# Ví d??? ngu???i 1 mua 30 items, ngu???i 2 mua 3 items...(row sums)
rtl_size
max(rtl_size)
hist(rtl_size,breaks = 100, prob = T)
itemFrequencyPlot(rtl,support=0.01,cex.names=0.5) #Plot d??? xem 1 item su???t hi???n or du???c mua bao nhiêu l???n 
rules=apriori(rtl)
rules=apriori(rtl,parameter = list(supp=0.005,conf=0.8))
plot(rules)                  
plot(rules,measure = c("support","lift"),shading = "confidence")
rules1=subset(rules,subset=lift>1.2)
inspect(head(rules1,n=3,by="confidence"))
interestMeasure(head(rules,n=3))
