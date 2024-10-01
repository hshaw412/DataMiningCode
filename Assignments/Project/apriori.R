# Install and load necessary libraries
#install.packages("caret")
#install.packages("arules")
#install.packages("arulesViz")
library(caret)
library(arules)
library(arulesViz)

# Read the CSV file
data <- read.csv("~/Desktop/UNI/Year 4/Tri 2/DataMining/DataMiningCode/Assignments/Project/Data/preprocessed_data.csv", sep = ",")

# View the first 20 rows
head(data, 20)

# Define categorical columns
categorical_columns <- c('Marital.status', 'Application.mode', 'Application.order', 'Course',
                         'Daytime.evening.attendance', 'Previous.qualification', 'Nacionality',
                         'Mother.s.qualification', 'Father.s.qualification', 'Mother.s.occupation',
                         'Father.s.occupation', 'Displaced', 'Educational.special.needs', 'Debtor',
                         'Tuition.fees.up.to.date', 'Gender', 'Scholarship.holder', 'International', 
                         'Target', 'Age.group', 'Unemployment.rate.bins')

# Subset the data with categorical columns
data_categorical <- data[categorical_columns]

# Convert categorical columns to factors
data_categorical[] <- lapply(data_categorical, as.factor)

# One-hot encode the data
#data_one_hot <- as.data.frame(model.matrix(~ . - 1, data = data_categorical))

dummy_model <- dummyVars(~ ., data = data_categorical)
data_one_hot <- predict(dummy_model, newdata = data_categorical)
data_one_hot <- as.data.frame(data_one_hot)

# Apply the Apriori algorithm with a minimum support of 0.05 and maxlen of 3
frequent_itemsets <- apriori(data_one_hot, parameter = list(support = 0.05, minlen=1, maxlen = 3, target="frequent itemsets"))

# Generate association rules with a minimum confidence of 0.7
rules <- apriori(data_one_hot, parameter = list(support = 0.03, confidence = 0.7, minlen=1, maxlen = 3, target="rules"))
rules <- rules[!is.redundant(rules)]
rules <- subset(rules, size(lhs(rules)) > 0)

# Sort rules by confidence
sorted_rules <- sort(rules, by = "confidence", decreasing = TRUE)

# Save sorted rules to a CSV file
write(sorted_rules, file = "~/Desktop/UNI/Year 4/Tri 2/DataMining/DataMiningCode/Assignments/Project/Data/found_rules_5_70_3R.csv", sep = ",", quote = TRUE, row.names = FALSE)

# View the rules
inspect(head(sorted_rules))
