> myAdult <- read_delim (
     'C:/Users/moham/Documents/adult.data', 
     delim = ", ", 
     col_names = c (
         "age", 
         "workClass", 
         "finalWeight", 
         "education", 
         "educationNum", 
         "maritalStatus", 
         "occupation", 
         "relationship", 
         "race", 
         "sex", 
         "capitalGain", 
         "capitalLoss", 
         "hoursPerWeek",  
         "nativeCountry", 
         "income"
     )
 )


> nrow(myAdult)

> ncol(myAdult)

> myAdult[myAdult == "?"] <- NA

> unique(myAdult$age)
> unique(myAdult$workClass)
> unique(myAdult$finalWeight)
> unique(myAdult$education)
> unique(myAdult$educationNum)
> unique(myAdult$maritalStatus)
> unique(myAdult$occupation)
> unique(myAdult$relationship)
> unique(myAdult$race)
> unique(myAdult$sex)
> unique(myAdult$capitalGain)
> unique(myAdult$capitalLoss)
> unique(myAdult$hoursPerWeek)
> unique(myAdult$nativeCountry)
> unique(myAdult$income)

> str(myAdult)

> sum(is.na(myAdult))

> sum(!complete.cases(myAdult))

> install.packages("DataExplorer")
> library("DataExplorer")
> plot_missing(myAdult)

> sum(apply(X = is.na(myAdult), MARGIN = 1, FUN = mean) > 0.25)

> myAdult <- subset(myAdult, select = c("age", "workClass", "educationNum", "maritalStatus", "occupation", "relationship", "race", "sex", "capitalGain", "capitalLoss", "hoursPerWeek", "nativeCountry", "income"))

> install.packages("ggplot2")
> install.packages("ggplot2")
> library("ggplot2")


> ggplot(data = myAdult, aes(x = age)) + geom_histogram(binwidth = 5, fill = "pink", color = "black") + labs(x = "Age", y = "Frequency")

> qAge <- quantile(myAdult$age, probs = c(0.25,0.75))
> iqrAge <- IQR(myAdult$age)
> lowerAge <- qAge[1] - (1.5 * iqrAge)
> upperAge <- qAge[2] + (1.5 * iqrAge)
> ageOutliersPercentage <- length(myAdult$age[myAdult$age < lowerAge | myAdult$age > upperAge]) / length(myAdult$age) * 100
> ageOutliersPercentage

> boxplot(myAdult$age, main = "Age")

> sum(is.na(myAdult$age)) / length(myAdult$age) * 100

> myAdult <- subset(myAdult, age >= max(summary(myAdult$age)[1], lowerAge) & age <= min(summary(myAdult$age)[6], upperAge))

> boxplot(myAdult$age, main = "Age after removing outliers")

> ageOutliersPercentage <- length(myAdult$age[myAdult$age < lowerAge | myAdult$age > upperAge]) / length(myAdult$age) * 100
> ageOutliersPercentage

> ggplot(data = myAdult, aes(x = workClass)) + geom_bar(fill = "pink", color = "black") + labs(x = "Work Class", y = "Frequency")

> myAdult$workClass[myAdult$workClass == 'Never-worked' | myAdult$workClass == 'Without-pay' ] <- 'No-Payment'
> myAdult$workClass[myAdult$workClass == 'Federal-gov' | myAdult$workClass == 'State-gov' | myAdult$workClass == 'Local-gov'] <- 'Government'
> myAdult$workClass[myAdult$workClass == 'Self-emp-inc' | myAdult$workClass == 'Self-emp-not-inc'] <- 'Self-Employed'
> unique(myAdult$workClass)

> ggplot(data = myAdult, aes(x = workClass)) + geom_bar(fill = "pink", color = "black") + labs(x = "Work Class after grouping", y = "Frequency")

> sum(is.na(myAdult$workClass)) / length(myAdult$workClass) * 100

> myAdult$workClass[is.na(myAdult$workClass)] <- names(which.max(table(myAdult$workClass)))
> sum(is.na(myAdult$workClass)) / length(myAdult$workClass) * 100

> ggplot(data = myAdult, aes(x = workClass)) + geom_bar(fill = "pink", color = "black") + labs(x = "Work Class after filling NA", y = "Frequency")

> ggplot(data = myAdult, aes(x = educationNum)) + geom_histogram(binwidth = 5, fill = "pink", color = "black") + labs(x = "Education Number", y = "Frequency")

> sum(is.na(myAdult$educationNum)) / length(myAdult$educationNum) * 100

> unique(myAdult$educationNum)

> myAdult$educationNum <- cut(myAdult$educationNum, breaks = c(0, 8, 10, 12, 14, Inf), labels = c("Elementary", "High School", "Some College", "Associate", "Higher Degree"), right = FALSE)
> unique(myAdult$educationNum)

> ggplot(data = myAdult, aes(x = maritalStatus)) + geom_bar(fill = "pink", color = "black") + labs(x = "Marital Status", y = "Frequency")

> ggplot(data = myAdult, aes(x = occupation)) + geom_bar(fill = "pink", color = "black") + labs(x = "Occupation", y = "Frequency")

> sum(is.na(myAdult$occupation)) / length(myAdult$occupation) * 100

> myAdult$ occupation[is.na(myAdult$occupation)] <- names(which.max(table(myAdult$occupation)))
> sum(is.na(myAdult$occupation)) / length(myAdult$occupation) * 100

> ggplot(data = myAdult, aes(x = occupation)) + geom_bar(fill = "pink", color = "black") + labs(x = "Occupation after filling NA", y = "Frequency")

> ggplot(data = myAdult, aes(x = relationship)) + geom_bar(fill = "pink", color = "black") + labs(x = "Relationsship", y = "Frequency")

> ggplot(data = myAdult, aes(x = race)) + geom_bar(fill = "pink", 
  color = "black") + labs(x = "Race", y = "Frequency")

> sum(complete.cases(myAdult$race) & myAdult$race == "White") /
  length(myAdult$race) * 100

> myAdult <- subset(myAdult, select = -race)

> ggplot(data = myAdult, aes(x = sex)) + geom_bar(fill = "pink", color = "black") + labs(x = "Sex", y = "Frequency")

> ggplot(data = myAdult, aes(x = capitalGain)) + geom_histogram(binwidth = 4000, fill = "pink", color = "black") + labs(x = "Capital Gain", y = "Frequency")

> boxplot(myAdult$capitalGain, main = "Capital Gain")

> qCapGain <- quantile(myAdult$capitalGain, probs = c(0.25,0.75))
> iqrCapGain <- IQR(myAdult$capitalGain)
> lowerCapGain <- qCapGain[1] - (1.5 * iqrCapGain)
> upperCapGain <- qCapGain[2] + (1.5 * iqrCapGain)
> capGainOutliersPercentage <- length(myAdult$capitalGain[myAdult$capitalGain < lowerCapGain | myAdult$capitalGain > upperCapGain]) / length(myAdult$capitalGain) * 100
> capGainOutliersPercentage

> gainBins <- ntile(myAdult$capitalGain, n = 15)
> meanGainbins <- replicate(15,0)
> for(i in 1 : 15)
    myAdult$capitalGain[gainBins==i] <- mean(myAdult$capitalGain[gainBins==i])

> myAdult <- subset(myAdult, select = -capitalGain)

> ggplot(data = myAdult, aes(x = capitalLoss)) + geom_histogram(binwidth = 400, fill = "pink", color = "black") + labs(x = "Capital Loss", y = "Frequency")

> qCapLoss <- quantile(myAdult$capitalLoss, probs = c(0.25,0.75))
> iqrCapLoss <- IQR(myAdult$capitalLoss)
> lowerCapLoss <- qCapLoss[1] - (1.5 * iqrCapLoss)
> upperCapLoss <- qCapLoss[2] + (1.5 * iqrCapLoss)
> capLossOutliersPercentage

> myAdult <- subset(myAdult, capitalLoss >= max(summary(myAdult$capitalLoss)[1], lowerCapLoss) & capitalLoss <= min(summary(myAdult$capitalLoss)[6], upperCapLoss)) 

> boxplot(myAdult$capitalLoss, main = "Capital Loss after removing outliers")

> myAdult <- subset(myAdult, select = -capitalLoss)

> ggplot(data = myAdult, aes(x = hoursPerWeek)) + geom_histogram(binwidth = 10, fill = "pink", color = "black") + labs(x = "Hours Per Week", y = "Frequency")

> boxplot(myAdult$hoursPerWeek, main = "Hours per week")

> qHpw <- quantile(myAdult$hoursPerWeek, probs = c(0.25,0.75))
> iqrHpw <- IQR(myAdult$hoursPerWeek)
> lowerHpw <- qHpw[1] - (1.5 * iqrHpw)
> upperHpw <- qHpw[2] + (1.5 * iqrHpw)
> hpwOutliersPercentage <- length(myAdult$hoursPerWeek[myAdult$hoursPerWeek < lowerHpw | myAdult$hoursPerWeek > upperHpw]) / length(myAdult$hoursPerWeek) * 100
> hpwOutliersPercentage

> gainBins <- ntile(myAdult$hoursPerWeek, n = 15)
> meanGainbins <- replicate(15,0)
> for(i in 1 : 15)
    myAdult$hoursPerWeek[gainBins==i] <- mean(myAdult$hoursPerWeek[gainBins==i])

> boxplot(myAdult$hoursPerWeek,main="Hours per week after removing outliers")

> ggplot(data = myAdult, aes(x = nativeCountry)) + geom_bar(fill = "pink", color = "black") + labs(x = "Native Country", y = "Frequency")

> sum(complete.cases(myAdult$nativeCountry) & myAdult$nativeCountry == "United-States") / length(myAdult$nativeCountry) * 100

> myAdult <- subset(myAdult, select = -nativeCountry)

> standardize <- function(x, nMin, nMax) { return ((x - min(x)) /(max(x) - min(x)) * (nMax - nMin) + nMin) }
> myAdult$age <- standardize(myAdult$age, 0, 1)
> myAdult$hoursPerWeek <- standardize(myAdult$hoursPerWeek, 0, 1)

> myAdult$income <- as.factor(myAdult$income)
> class(myAdult$income)
> myAdult$income

> str(myAdult)

> ggplot(data = myAdult, aes(x = income)) + geom_bar(fill = "pink", color = "black") + labs(x = "income", y = "Frequency")

> table(myAdult$income)

> sum(myAdult$income == '<=50K') / length(myAdult$income) * 100
# our data is biased to <=50K class
