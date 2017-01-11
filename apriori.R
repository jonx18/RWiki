#experimento 5
library("arules")
library(arulesViz)

itemFrequencyPlot(trans, support=0.01)
rules <- apriori(trans, parameter=list(supp=0.1,conf=0.1))
summary(rules)
inspect(rules)
rules.sorted <- sort(rules)
inspect(rules.sorted)
interestMeasure(rules, c("support", "chiSquare", "confidence",
                         "conviction", "cosine", "leverage", "lift", "oddsRatio"), trans)
sapply(method, FUN = function(m) interestMeasure(x, m,
                                                 transactions, reuse, ...))

rules <- apriori(trans, parameter=list(supp=0.01,conf=0.01, minlen=2))
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned
summary(rules)
inspect(rules)
rules.sorted <- sort(rules, by="confidence", decreasing=TRUE)
inspect(rules.sorted)
df.rules <- as(rules, "data.frame") 
df.rules[order(df.rules$confidence,df.rules$lift, decreasing = TRUE), ]

plot(rules,method="graph",interactive=TRUE,shading=NA)
interestMeasure(rules, c("support", "chiSquare", "confidence",
                         "conviction", "cosine", "leverage", "lift", "oddsRatio"), trans)