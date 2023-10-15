install.packages("ClusterR") 
install.packages("cluster")

library(ClusterR) 
library(cluster)

set.seed(42)

num_points <- 200

feature1 <- c(rnorm(num_points, mean = 10, sd = 2), rnorm(num_points, mean = 20, sd = 2))
feature2 <- c(rnorm(num_points, mean = 30, sd = 2), rnorm(num_points, mean = 15, sd = 2))
feature3 <- c(rnorm(num_points, mean = 5, sd = 1.5), rnorm(num_points, mean = 25, sd = 1.5))
feature4 <- c(rnorm(num_points, mean = 18, sd = 3), rnorm(num_points, mean = 10, sd = 3))

target <- sample(c("Y", "N"), num_points * 2, replace = TRUE)

data <- data.frame(Feature1 = feature1, Feature2 = feature2, Feature3 = feature3, Feature4 = feature4, Target=target)

head(data)

write.csv(data, "data.csv", row.names = FALSE)
data <- read.csv("data.csv")

head(data)

#data_1 <- data[,-1]
df <- subset(data, select = -Target)
head(df)
km <- kmeans(df, centers = 3, nstart = 20) 
km 

km$cluster 

cm <- table(data$Target, km$cluster) 
cm 

 
plot(df[c("Feature1", "Feature2")],  
     col = km$cluster,
     pch=19,
     main = "K-means with 3 clusters") 


km$centers 
km$centers[, c("Feature1", "Feature2")] 

y_kmeans <- km$cluster 
clusplot(df[, c("Feature1", "Feature2")], 
         y_kmeans, 
         lines = 0, 
         shade = TRUE, 
         color = TRUE, 
         labels = 2, 
         plotchar = FALSE, 
         span = TRUE, 
         main = paste("Cluster"), 
         xlab = 'Feature1', 
         ylab = 'Feature2') 
