install.packages("arules")
install.packages("arulesViz")
install.packages(factoextra)
library(arulesViz)
library(arules)
library(factoextra)

groceries<- read.csv("GroceriesInitial.csv", header=TRUE,sep = ",",stringsAsFactors = TRUE)

product_names <- levels(unlist(groceries[ ,4:35]))
blank <- which(product_names=="") 
product_names <- product_names[-c(blank)] 
products <- as.data.frame(t(apply(groceries[,4:35],1, function(x)
  (product_names) %in% as.character(unlist(x)))))

names(products) <- product_names
groceries_binary <- cbind(groceries[,1:3],products)


groceries_binary <- groceries_binary[c("id","basket_value","recency_days","citrus fruit","tropical fruit",
                                       "whole milk", "other vegetables", "rolls/buns","chocolate", "bottled water", 
                                       "yogurt", "sausage", "root vegetables", "pastry", "soda", "cream")]

groceries_discrete <- groceries_binary
cut_points <- quantile(groceries_discrete$basket_value
                       ,probs = c(0,0.33, 0.66,1)
                       ,na.rm = TRUE
                       ,names = FALSE)

groceries_discrete$basket_value_desc <- cut(groceries_discrete$basket_value
                                            ,breaks = cut_points,labels=c("low_value_basket","medium_value_basket","high_value_basket")
                                            ,include.lowest = TRUE)
table(groceries_discrete$basket_value_desc)

temp <- levels(unlist(groceries_discrete[c("basket_value_desc")]))

basket_values_df <- as.data.frame(t(apply(groceries_discrete[c("basket_value_desc")],1, function(x)
  (temp) %in% as.character(unlist(x)))))
names(basket_values_df)<- temp
groceries_discrete <- cbind(groceries_discrete[,1:16],basket_values_df)


############################
rules1 <- apriori(groceries_binary[,4:ncol(groceries_binary)]
                  , parameter = list(minlen=2, sup=0.01, conf=0)
                  , control = list(verbose=FALSE) )
rules_sorted1 <- sort(rules1, decreasing = TRUE, by = "confidence")
inspect(rules_sorted1[1:20])

rules2 <- apriori(groceries_discrete[,4:ncol(groceries_discrete)]
                 , parameter = list(minlen=2, sup=0.01, conf=0)
                 , control = list(verbose=FALSE) )
rules_sorted2 <- sort(rules2, decreasing = TRUE, by = "confidence")
inspect(rules_sorted2[1:20])

#############################
set.seed(1234)
groceries_scaled <- scale(groceries_binary[c("basket_value", "recency_days")], center=TRUE, scale=TRUE)

kmeans_fit <- kmeans(groceries_scaled,5,nstart=25,iter.max = 1000)
kmeans_fit

pc <- prcomp(groceries_scaled)
plot(pc, type='1')
summary(pc)

#just to "see" the PCA
comp <- data.frame(pc$x[,1:2])
plot(comp, pch=16, col=rgb(0,0,0,0.5)) 


kmeans_fit$centers
centers <- t(apply(kmeans_fit$centers, 1, function(r)
  r * attr(groceries_scaled, 'scaled:scale') + attr(groceries_scaled, 'scaled:center')))
centers

pie_data <- table(kmeans_fit$cluster)
pie_data <- pie_data/sum(pie_data)*100
pie(pie_data,labels = paste(names(pie_data), "\n", pie_data, sep=""))

factor_clusters <- as.data.frame(as.factor(kmeans_fit$cluster))
cluster_names <- levels(unlist(factor_clusters))
clusters <- as.data.frame(t(apply(factor_clusters, 1, function(x) (cluster_names) 
                                  %in% as.character(unlist(x)))))
names(clusters) <-list("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5")
groceries_discrete <- cbind(groceries_discrete, clusters)
groceries_binary <- cbind(groceries_binary, clusters)

####################
rules_final <- apriori(groceries_binary[,4:ncol(groceries_binary)]
                   , parameter = list(minlen=2, sup=0.01, conf=0)
                   , control = list(verbose=FALSE) )
rules_final_sorted <- sort(rules_final, decreasing = TRUE, by = "confidence")
inspect(rules_final_sorted[1:20])

