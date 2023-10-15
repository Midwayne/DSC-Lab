set.seed(200) 

num_observations <- 1000

predictor1 <- rnorm(num_observations, mean = 50, sd = 10)
predictor2 <- rnorm(num_observations, mean = 30, sd = 5)
predictor3 <- rnorm(num_observations, mean = 70, sd = 15)

target <- ifelse(predictor1 + predictor2 + predictor3 > 150, 1, 0)

data <- data.frame(
  Predictor1 = predictor1,
  Predictor2 = predictor2,
  Predictor3 = predictor3,
  Target = target
)

write.csv(data, "sample_data.csv", row.names = FALSE)
data <- read.csv("sample_data.csv")

head(data)

install.packages("caTools")
install.packages("randomForest")

library(caTools)
library(randomForest)

split <- sample.split(data, SplitRatio = 0.7)

train <- subset(data, split=="TRUE")
test <- subset(data, split=="FALSE")

rf_model <- randomForest(Target ~ Predictor1 + Predictor2 + Predictor3, data = train)

rf_model

y_pred = predict(rf_model, newdata = test[-4]) 


confusion_mtx = table(test[, 4], y_pred) 
confusion_mtx 

plot(rf_model) 

importance(rf_model) 

varImpPlot(rf_model) 
