#####################################################################################
# Classification Algorithms: K-Nearest_Neighbor
####################################################################################

# Notes:
# If k is too low, then the model is overfitting, and vice versa.
# A sign of an over fitting model is seen in accurate predictions in the training data, 
# and inaccurate predictions in the test data.
# A sign of an under fitting model is seen in the case of comparable training data and
# test data.

########################################################################################
#  Step 1. Read and Prepare the data 
#  Split the data - Training set and Validation set 
########################################################################################

# import the CSV file
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE) 

str(wbcd) # Lets check what we have in this dataset

#First column is the randomly generated Patient ID
#Get rid of it
#wbcd <- wbcd[-1]
wbcd$id <- NULL

# table or proportions with more informative labels
table(wbcd$diagnosis)

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), 
                         labels = c("Benign", "Malignant"))

#prop() calculates the proportion of a value or category in a variable
round(prop.table(table(wbcd$diagnosis)) * 100, 1)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#Given substantial scale differences, we need to normalize the data
#Convert all values on a 0 to 1 scale
#Function for Min-Max Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Applying normalization function to all columns except the response variable
wbcd_n <- as.data.frame(lapply(wbcd[2:ncol(wbcd)], normalize))

#Checking whether scale differences persist
summary(wbcd_n[c("radius_mean", "area_mean", "smoothness_mean")])

# Creating training and testing sets. 
# Lets have 100 rows in test, rest in train

#Set a seed for random number generator for consistent output
set.seed(123)

#Selects 100 random rows for test data
test_set <- sample(1:nrow(wbcd_n), 100) 
# Depending on R-version and computer, different rows may be selected. 
# If that happens, results are different. 

# Create a train set and test set
#First the predictors
wbcd_train <- wbcd_n[-test_set, ]
wbcd_test <- wbcd_n[test_set, ]

#Now the response (aka Labels)
wbcd_train_labels <- wbcd[-test_set, "diagnosis"]
wbcd_test_labels <- wbcd[test_set, "diagnosis"]

#Lets run the KNN command
library(class)
library(caret)

#Run KNN on train data, create predictions for test data
#Starting K value close to sqrt(nrow(wbcd_train))
sqrt(nrow(wbcd_train))

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

#Evaluate model results
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)

#True Positives (TP): These are cases in which we predicted yes (they have the disease), and they do have the disease.
#True Negatives (TN): We predicted no, and they don't have the disease.
#False Positives (FP): We predicted yes, but they don't actually have the disease. (Also known as a "Type I error.")
#False Negatives (FN): We predicted no, but they actually do have the disease. (Also known as a "Type II error.")

# Accuracy: Overall, how often is the classifier correct?
(62 + 33) / (62 + 33 + 5 + 0)

# Error rate: Overall, how often is it wrong?
(5 + 0) / (62 + 33 + 5 + 0)

# error rate = 1 - accuracy
1 - 0.95

## Beyond accuracy: other performance measures ----

# Kappa statistic: This is essentially a measure of how well the classifier performed as compared to how well it would have performed simply by chance.
pr_a <- (0.62 + 0.33)
pr_a

pr_e <- 0.62*0.67 + 0.38*0.33
pr_e

k <- (pr_a - pr_e) / (1 - pr_e)
k

# Sensitivity: When it's actually yes, how often does it predict yes?
sens <- 33 / (33 + 5)
sens

# Specificity: When it's actually no, how often does it predict no?
spec <- 62 / (62 + 0)
spec

# Precision: When it predicts yes, how often is it correct?
prec <- 33 / (33 + 0)
prec

# example using the caret package
confusionMatrix(wbcd_test_pred, wbcd_test_labels, positive = "Malignant")


#Using Z-Score Normalization
wbcd_z <- as.data.frame(scale(wbcd[2:ncol(wbcd)]))
wbcd_train <- wbcd_z[-test_set, ]; wbcd_test <- wbcd_z[test_set, ]  
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)
confusionMatrix(wbcd_test_pred, wbcd_test_labels, positive = "Malignant")


# Partioning Data Randomly using caret, 70% in train
set.seed(567)
in_train <- createDataPartition(wbcd$diagnosis, p = 0.7, list = FALSE)
wbcd_train <- wbcd_n[in_train, ]
wbcd_test <- wbcd_n[-in_train, ]
wbcd_train_labels <- wbcd[in_train, "diagnosis"]
wbcd_test_labels <- wbcd[-in_train, "diagnosis"]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, 
           prop.chisq=FALSE)
confusionMatrix(wbcd_test_pred, wbcd_test_labels, positive = "Malignant")
