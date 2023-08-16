> dataFrame <- data.frame(workclass = head(myAdult$workClass , n = 100), income = head(myAdult$occupation, n = 100))
> transactions <- as(dataFrame , "transactions")
> frequentSet <- eclat(transactions, parameter = list(support = 0.2))
> inspect(frequentSet)
