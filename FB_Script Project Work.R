restaurant <- read.csv("Restaurant_revenue.csv")
View(restaurant)

#EDA:

summary(restaurant) 
str(restaurant)

restaurant$Cuisine_Type <- as.factor(restaurant$Cuisine_Type)
restaurant$Promotions <- as.factor(restaurant$Promotions)

summary(restaurant) 
str(restaurant)


is.na(restaurant)
sum(is.na(restaurant)) 


layout(matrix(1:2, nrow = 1))
barplot(table(restaurant$Cuisine_Type), main = "Cuisine Type")
barplot(table(restaurant$Promotions), main = "Promotions")
dev.off()


hist(restaurant$Number_of_Customers,
     xlab = "Number of Customers (in classes)",
     main = "Histogram of restaurant Customers")

hist(restaurant$Number_of_Customers,
     breaks = 20,
     xlab = "Number of Customers (in classes)",
     main = "Histogram of restaurant Customers")


hist(restaurant$Menu_Price, 
     xlab = "Menu prices (in classes)", 
     main = "Histogram of Menu prices")

hist(restaurant$Menu_Price, 
     breaks = 15,
     xlab = "Menu prices (in classes)", 
     main = "Histogram of Menu prices")


hist(restaurant$Marketing_Spend, 
     xlab = "Marketing spending (in classes)", 
     main = "Histogram of Marketing spending")

hist(restaurant$Marketing_Spend,
     breaks = 17,
     xlab = "Marketing spending (in classes)", 
     main = "Histogram of Marketing spending")


hist(restaurant$Average_Customer_Spending,
     xlab = "Average Customer Spending (in classes)", 
     main = "Histogram of Average Customer Spending")

hist(restaurant$Average_Customer_Spending,
     breaks = 17,
     xlab = "Average Customer Spending (in classes)", 
     main = "Histogram of Average Customer Spending")


hist(restaurant$Reviews,
     xlab = "Reviews (in classes)", 
     main = "Histogram of Rieviews")

hist(restaurant$Reviews,
     breaks = 16,
     xlab = "Reviews (in classes)", 
     main = "Histogram of Rieviews")


hist(restaurant$Monthly_Revenue,
     xlab = "Monthly Revenue (in classes)", 
     main = "Histogram of Monthly Revenue")

hist(restaurant$Monthly_Revenue,
     breaks = 20,
     xlab = "Monthly Revenue (in classes)", 
     main = "Histogram of Monthly Revenue")


layout(matrix(1:6, nrow = 3))

hist(restaurant$Number_of_Customers,
     xlab = "Number of Customers (in classes)",
     main = "Histogram of restaurant Customers")

hist(restaurant$Menu_Price, 
     xlab = "Menu prices (in classes)", 
     main = "Histogram of Menu prices")

hist(restaurant$Marketing_Spend, 
     xlab = "Marketing spending (in classes)", 
     main = "Histogram of Marketing spending")

hist(restaurant$Average_Customer_Spending,
     xlab = "Average Customer Spending (in classes)", 
     main = "Histogram of Average Customer Spending")

hist(restaurant$Reviews,
     xlab = "Reviews (in classes)", 
     main = "Histogram of Reviews")

hist(restaurant$Monthly_Revenue,
     xlab = "Monthly Revenue (in classes)", 
     main = "Histogram of Monthly Revenue")

dev.off()


tapply(X = restaurant , INDEX = restaurant$Cuisine_Type, FUN = summary)

tapply(X = restaurant , INDEX = restaurant$Promotions, FUN = summary)



library(ggplot2)
library(dplyr)

cor_matrix <- cor(select(restaurant, -Cuisine_Type, -Promotions))

cor_matrix_long <- as.data.frame(as.table(cor_matrix))
colnames(cor_matrix_long) <- c("Variable1", "Variable2", "Correlation")

ggplot(cor_matrix_long, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap")


plot(
  restaurant$Promotions,
  restaurant$Number_of_Customers,
  main = "Number of Customers vs. Promotions",
  xlab = "Promotions",
  ylab = "Number of Customers"
)


coltable <- c("black", "yellow", "red", "lightblue")


plot(
  jitter(restaurant$Number_of_Customers),
  jitter(restaurant$Monthly_Revenue),
  pch = 19,
  col = coltable[as.integer(restaurant$Cuisine_Type)]
)


plot(
  jitter(restaurant$Menu_Price),
  jitter(restaurant$Number_of_Customers),
  pch = 19,
  col = coltable[as.integer(restaurant$Cuisine_Type)]
)


coltable_1 <- c("red", "blue")


plot(
  jitter(restaurant$Menu_Price),
  jitter(restaurant$Monthly_Revenue),
  pch = 19,
  col = coltable_1[as.integer(restaurant$Promotions)]
)

y <- restaurant[restaurant$Cuisine_Type == "Italian", ]
plot(y$Menu_Price, y$Number_of_Customers, pch = 19)


plot(y$Average_Customer_Spending, y$Monthly_Revenue, pch = 19)
plot(y$Number_of_Customers, y$Monthly_Revenue, pch = 19)

#PCA:

restaurant.scaled <- scale(restaurant[, c(1:3, 5, 7, 8)])
restaurant.pca <- prcomp(restaurant.scaled)
summary(restaurant.pca)


head(restaurant.pca$x)
restaurant.pca$x[, 1:5]

head(restaurant.pca$x[, 1:5])

#Hierarchical Clustering:

restaurant.pca.dist <- dist(restaurant.pca$x[, 1:5])
restaurant.pca.hc <- hclust(restaurant.pca.dist)
plot(restaurant.pca.hc) 

restaurant.pca.hc.clusters <- cutree(restaurant.pca.hc, 2)
restaurant.pca.hc.clusters

table(restaurant$Cuisine_Type, restaurant.pca.hc.clusters)
table(restaurant$Promotions, restaurant.pca.hc.clusters)

tapply(X = restaurant, INDEX = list(restaurant.pca.hc.clusters), FUN = summary)

library(fpc)
cluster_stats_result <- cluster.stats(restaurant.pca.dist, cluster = restaurant.pca.hc.clusters)

library(cluster)
sil <- silhouette(x = restaurant.pca.hc.clusters, dist = restaurant.pca.dist) 
summary(sil)

library(dbscan)
hullplot(x = restaurant[, 1:2], cl = restaurant.pca.hc.clusters)
hullplot(x = restaurant[, 2:3], cl = restaurant.pca.hc.clusters)
hullplot(x = restaurant[, c(3, 5)], cl = restaurant.pca.hc.clusters)
hullplot(x = restaurant[, c(1,8)], cl = restaurant.pca.hc.clusters)



#DBSCAN:


restaurant_dbs <- dbscan(restaurant[, c(1:3, 5, 7, 8)], minPts = 2, eps = 0.5)

restaurant_dbs

#Decision trees:

library(rpart)
library(rpart.plot)
nrow(restaurant)

restaurant.idx <- sample(x = nrow(restaurant), 800)
restaurant.idx

restaurant.train <- restaurant[restaurant.idx, ]
restaurant.test <- restaurant[-restaurant.idx, ]


table(restaurant.train$Cuisine_Type)
table(restaurant.test$Cuisine_Type)

table(restaurant.train$Promotions)
table(restaurant.test$Promotions)

restaurant.dt <-
  rpart(
    Promotions ~ Cuisine_Type + Menu_Price + Average_Customer_Spending + Monthly_Revenue,
    data = restaurant.train
  )

summary(restaurant.dt)

rpart.plot(restaurant.dt)

restaurant.test.pred <- predict(restaurant.dt, restaurant.test, type = "class")

restaurant.test.pred1 <- predict(restaurant.dt, restaurant.test)

apply(X = restaurant.test.pred1, MARGIN = 1, FUN = which.max)

cm <- table(restaurant.test$Promotions, restaurant.test.pred)

result <- (cm[1, 1] + cm[2, 2])/sum(cm)
cat("Accuracy coefficient: \n", result)





restaurant.dt_1 <-
  rpart(
    Cuisine_Type ~ Promotions + Menu_Price + Average_Customer_Spending + Monthly_Revenue,
    data = restaurant.train
  )

summary(restaurant.dt_1)

rpart.plot(restaurant.dt_1)

restaurant.test.pred_1 <-
  predict(restaurant.dt_1, restaurant.test, type = "class")

restaurant.test.pred2 <- predict(restaurant.dt_1, restaurant.test)

apply(X = restaurant.test.pred2, MARGIN = 1, FUN = which.max)

cm_1 <- table(restaurant.test$Cuisine_Type, restaurant.test.pred_1)

result_1 <- (cm_1[1, 1] + cm_1[2, 2] + cm_1[3, 3] + cm_1[4, 4]) / sum(cm_1)
cat("Accuracy coefficient: \n", result_1)


#Application of bayes classifier:

library(e1071)

restaurant.bayes <-
  naiveBayes(Promotions ~ Cuisine_Type, data = restaurant.train)

rest.pred <- predict(object = restaurant.bayes, newdata = restaurant.test)
rest.pred 

out <- table(restaurant.test$Promotions, rest.pred)
out

result_2 <- (out[1, 1] + out[2, 2])/sum(out)
cat("Accuracy coefficient: \n", result_2)

#(multivariate) linear regression model:


restaurant.regr <-
  lm(
    Monthly_Revenue ~ Number_of_Customers + Menu_Price + Marketing_Spend + Average_Customer_Spending + Reviews,
    data = restaurant
  )

summary(restaurant.regr)
anova(restaurant.regr) 

yp <- fitted(restaurant.regr)


plot(
  restaurant$Number_of_Customers,
  restaurant$Monthly_Revenue, pch = 19
)

par(new = T)

plot(restaurant$Number_of_Customers,
     yp, pch = 19, col = "red"
)



plot(restaurant.regr$residuals, pch = 19)

abline(h = 0, lwd = 2, col = "red")

qqnorm(restaurant.regr$residuals)
qqline(restaurant.regr$residuals, col = "yellow", lwd = 2)


# Create a multivariate polynomial regression model:
restaurant.regr2 <- lm(
  Monthly_Revenue ~ poly(Number_of_Customers, degree = 2, raw = TRUE) +
    poly(Menu_Price, degree = 2, raw = TRUE) +
    poly(Marketing_Spend, degree = 2, raw = TRUE) +
    poly(Average_Customer_Spending, degree = 2, raw = TRUE) +
    poly(Reviews, degree = 2, raw = TRUE),
  data = restaurant
)


summary(restaurant.regr2)

#ANOVA (comparing the model):

anova(restaurant.regr, restaurant.regr2)

#AIC:

AIC(restaurant.regr)
AIC(restaurant.regr2)
