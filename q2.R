install.packages("arules") 
install.packages("arulesViz")

library(arules)
library(arulesViz)


fruits <- c("Apple", "Banana", "Orange", "Mango", "Pineapple", "Grapes", "Strawberry", "Kiwi", "Pear", "Cherry")

num_transactions <- 1000  

set.seed(42)  
transactions <- lapply(1:num_transactions, function(i) {
  num_items <- sample(1:5, 1)  # Number of fruits in each transaction (1 to 5 fruits)
  items <- sample(fruits, num_items, replace = TRUE)
  items <- sort(items)  # Sort items for consistency
  items
})

transactions <- as(transactions, "transactions")

inspect(transactions[1:5])

transactions_df <- as.data.frame(as(transactions, "matrix"))

write.csv(transactions_df, "fruit_transactions.csv", row.names = FALSE)


transactions_df <- read.csv("fruit_transactions.csv", stringsAsFactors = FALSE)

# Convert the data frame back to transactions
data <- as(transactions_df, "transactions")
data <- subset(data, select = -transactionID)


inspect(data[1:5])

set.seed = 200
apriori_model = apriori(data = data,  
                        parameter = list(support = 0.004,  
                                         confidence = 0.2)) 

itemFrequencyPlot(data, topN = 10) 

inspect(sort(apriori_model, by = 'lift')[1:10]) 
plot(apriori_model, method = "graph",  
     measure = "confidence", shading = "lift") 
