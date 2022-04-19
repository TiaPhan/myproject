library('arules')
data("AdultUCI")
uci=AdultUCI
uci[1:2,]
uci[1:2]
uci[["fnlwgt"]] = NULL
uci[["education-num"]] = NULL
uci[["age"]] = ordered(cut(uci[["age"]], c(15,25,45,65,100)), labels=c("Young","Middle-age","Senior","Old"))
uci[["hours-per-week"]] = ordered(cut(uci[["hours-per-week"]], c(0,25,40,60,168)), labels=c("Part-time", "Full-time","Overtime","Workaholic"))
uci[["capital-gain"]] = ordered(cut(uci[["capital-gain"]], c(-Inf,0, median(uci[["capital-gain"]][uci[["capital-gain"]]>0]), Inf)), labels=c("None", "Low", "High"))
uci[["capital-loss"]] = ordered(cut(uci[["capital-loss"]],c(-Inf,0,median(uci[["capital-loss"]][uci[["capital-loss"]]>0]),Inf)), labels=c("None", "Low", "High"))
str(uci)
adult=as(uci,"transactions")
summary(adult)
windows()
itemFrequencyPlot(adult)
rules=apriori(adult,parameter = list(supp=0.01, conf=0.06 ))
rules_income_small=subset(rules,subset = rhs%in% "income=small"&lift>1.2)
rules_income_large=subset(rules,subset = rhs%in% "income=large"&lift>1.2)
inspect(head(rules_income_small, n=3, by = "confidence"))
inspect(head(rules_income_large, n=3, by = "confidence"))
