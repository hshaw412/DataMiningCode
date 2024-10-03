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
dummy_model <- dummyVars(~ ., data = data_categorical)
data_one_hot <- predict(dummy_model, newdata = data_categorical)
data_one_hot <- as.data.frame(data_one_hot)

# Apply the Apriori algorithm with a minimum support and max itemset length
#frequent_itemsets <- apriori(data_one_hot, parameter = list(support = 0.01, minlen=1, maxlen = 2, target="frequent itemsets"))

# Timing
start_time = Sys.time()

# Generate association rules with a minimum confidence and max itemset length
rules <- apriori(data_one_hot, parameter = list(support = 0.01, confidence = 0.9, minlen=1, maxlen = 2, target="rules"))
rules <- rules[!is.redundant(rules)]
rules <- subset(rules, size(lhs(rules)) > 0)

print("Time taken: ", Sys.time() - start_time)

# Sort rules by confidence
sorted_rules <- sort(rules, by = "confidence", decreasing = TRUE)

# Save sorted rules to a CSV file
write(sorted_rules, file = "~/Desktop/UNI/Year 4/Tri 2/DataMining/DataMiningCode/Assignments/Project/Data/found_rules_1_90_2R.csv", sep = ",", quote = TRUE, row.names = FALSE)

# View the rules
#inspect(head(sorted_rules))
