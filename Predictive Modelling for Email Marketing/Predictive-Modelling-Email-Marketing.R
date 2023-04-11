
# List of all packages that need to be used

package_list <- c("tidyverse", "readr", "dplyr", "caTools", "ggplot2", "caret", "Matrix", "xgboost", "pROC", "CustomerScoringMetrics", "e1071", "randomForest", "ROSE", "party", "MASS", "tree", "maptree", "ROCR", "gridExtra", "kableExtra", "patchwork")

# Install them if not installed

installed_packages <- package_list %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(package_list[!installed_packages])
}

# Load the packages 

invisible(lapply(package_list, library, character.only = TRUE))

# Reading the data to understand current scenario and performance

emaildata <- read.csv("assignment_data.csv", stringsAsFactors=TRUE)

esum <- emaildata %>% 
   group_by(email_segment) %>%
   summarise(email_count = n(), visit_count = sum(visit)) %>%
  as.data.frame()

esum[2, ] <- esum[3,]
esum <- esum[-3, ]

total.emails <- sum(esum$email_count)
total.visits <- sum(esum$visit_count)
visit.rate <-round(total.visits/total.emails * 100, 2)

esum <- esum[-3,]

esum_long <- esum %>% pivot_longer(!email_segment, names_to = "count_category", values_to = "count")

esum.plot <- ggplot(esum_long) + 
  geom_col(aes(x = email_segment, y = count, fill = count_category), position = "dodge") + 
  theme(legend.position = "top") + theme(legend.title=element_blank())

esum_grob <- tableGrob(esum, rows = NULL)
grid.arrange(esum.plot, esum_grob, nrow = 2, heights = c(9,3))

print(paste("The visit rate is", visit.rate, "%"))


# Checking Structure

str(df)

# Checking Summary

summary(df)

# Checking for NA's

colSums(is.na(df))

# Making sure the data does not have duplicate rows

df<- distinct(df)

# Dropping columns

df$account <- NULL
df$spend <- NULL
df$Customer_ID <- NULL

# Ignoring "No-Email" as that does not help us gauge performance of email campaign

df <- df %>% filter(email_segment != "No E-Mail")

# Target variable needs to be a factor

df$visit <- as.factor(df$visit)

# Check the levels of Purchase Segment

levels(df$purchase_segment)

# Null values in purchase_segment can be changed to factors based on the purchase value

df$purchase_segment[df$purchase >= 0 & df$purchase < 100] <- "1) 0 - 100"
df$purchase_segment[df$purchase >= 100 & df$purchase < 200] <- "2) 100 - 200"
df$purchase_segment[df$purchase >= 200 & df$purchase < 350] <- "3) 200 - 350"
df$purchase_segment[df$purchase >= 350 & df$purchase < 500] <- "4) 350 - 500"
df$purchase_segment[df$purchase >= 500 & df$purchase < 750] <- "5) 500 - 750"
df$purchase_segment[df$purchase >= 750 & df$purchase < 1000] <- "6) 750 - 1,000"
df$purchase_segment[df$purchase >= 1000] <- "7) 1,000 +"

# Cleaning the purchase_segment column

df$purchase_segment <- as.character(df$purchase_segment)
df$purchase_segment <- gsub("[0-9]+) ","",df$purchase_segment)
df$purchase_segment  <- gsub(" ", "", df$purchase_segment)
df$purchase_segment  <- gsub(",", "", df$purchase_segment)
df$purchase_segment<- as.factor(df$purchase_segment)

# Changing other columns to factor

df$delivery <- as.factor(df$delivery)
levels(df$delivery) <- list("Home" = 1, "Work" = 2, "Multiple" = 3)

df$marriage <- as.factor(df$marriage)
levels(df$marriage) <- list("Married" = 1, "Single" = 2, "Other" = 0)

df$payment_card <- as.factor(df$payment_card)
levels(df$payment_card) <- list("Yes" = 1, "No" = 0)

df$employed <- as.factor(df$employed)
levels(df$employed) <- list("Yes" = 1, "No" = 0)

df$dependent <- as.factor(df$dependent)
levels(df$dependent) <- list("Yes" = 1, "No" = 0)

df$new_customer <- as.factor(df$new_customer)
levels(df$new_customer) <- list("New_Customer" = 1, "Existing_Customer" = 0)

df$mens <- as.factor(df$mens)
levels(df$mens) <- list("Purchased" = 1, "No_Purchase" = 0)

df$womens <- as.factor(df$womens)
levels(df$womens) <- list("Purchased" = 1, "No_Purchase" = 0)

df$phone <- as.factor(df$phone)
levels(df$phone) <- list("Yes" = 1, "No" = 0)


# Dropping purchase column

df$purchase <- NULL

# Checking summary of target variable for any imbalance 

summary(df$visit)

# Setting seed

set.seed(123)

# Shuffle the data set to ensure randomness

df <- df[sample(1:nrow(df)), ]
df <- as.data.frame(df)

# Splitting the data into training and test sets, here we are using a 70-30 split

split = sample.split(df$visit, SplitRatio = 0.70)

# Creating training set where split is true

trainingdata = subset(df, split == TRUE)

# Creating test set where split is false

testdata = subset(df, split == FALSE)

# Defining the variable for k-fold cross validation, here we take k = 5

train_control <- trainControl(method = "cv", number = 5)

# Oversampling the training set

oversampled.trainingdata <- ovun.sample(visit ~ . , data = trainingdata, method = "over", p= 0.5, seed=123)$data

# Summary of over-sampled target variable

summary(oversampled.trainingdata$visit)




# # Start Time
# 
# DT.start_time <- proc.time()[3]
# 
# # Build a decision tree
# 
# DT <- tree(visit ~ ., data = oversampled.trainingdata, control = tree.control(nrow(oversampled.trainingdata), mindev = 0))
# 
# # End Time
# 
# DT.stop_time <- proc.time()[3]
# 
# # Calculate run time
# 
# DT.time_elapsed <- DT.stop_time - DT.start_time
# DT.time_elapsed <- round(DT.time_elapsed, 2)

# Display run time

# print(paste("Decision Tree model took", DT.time_elapsed, "seconds to train."))

# 
# # Show Summary of decision tree
# 
# summary(DT)
# 
# # Predict on test data
# 
# predict_tree <- predict(DT, testdata, type="class")
# 
# # Confusion matrix
# 
# (dtree.confusion_matrix <- confusionMatrix(predict_tree, testdata$visit, positive='1', mode = "prec_recall"))
# 
# # Application of cv.tree (Cross Validation)
# 
# CVresults = cv.tree(DT, FUN = prune.tree, K = 5)
# 
# # Plotting the last 20 values
# 
# tree_size = tail(CVresults$size, 20)
# misclassifiations = tail(CVresults$dev, 20)
# 
# plot(tree_size, misclassifiations/nrow(oversampled.trainingdata), type = "b",
#      xlab = "Tree Size", ylab = "CV Misclassification Rate")
# 
# #Prune the tree
# 
# DT_prune = prune.misclass(DT, best = 3)
# 
# # Check the summary of the pruned tree
# 
# summary(DT_prune)
# 
# # Using Pruned Model for prediction
# 
# predict_tree_prune <- predict(DT_prune, testdata, type = "class")

# Confusion matrix

# cat("Decision Tree Confusion Matrix\n--------------------------------------\n")

# (dtree_prune.confusion_matrix <- confusionMatrix(predict_tree_prune, testdata$visit, positive='1', mode = "prec_recall"))


# Creating a dataframe to store model metrics

accuracy.df <- data.frame(Model = character(), 
                          Training_time_in_seconds = numeric(), 
                          Accuracy = numeric(), 
                          F1_score = numeric(),
                          Precision = numeric(),
                          Recall = numeric(), 
                          Sensitivity = numeric(), 
                          Specificity = numeric())

invisible(rbind(accuracy.df, list(Model = NA, Training_time_in_seconds = NA, Accuracy = NA, F1_score = NA, Precision = NA, Recall = NA, Sensitivity = NA, Specificity = NA)))

# accuracy.df[1,] <- c("Decision Tree", DT.time_elapsed,
                     round(dtree_prune.confusion_matrix$overall["Accuracy"]*100,2),
                     round(dtree_prune.confusion_matrix$byClass["F1"],2),
                     round(dtree_prune.confusion_matrix$byClass["Precision"]*100,2),
                          round(dtree_prune.confusion_matrix$byClass["Recall"]*100,2),
                          round(dtree_prune.confusion_matrix$byClass["Sensitivity"]*100,2),
                     round(dtree_prune.confusion_matrix$byClass["Specificity"]*100,2)
                          )

#--------------------------
# Random Forest
#--------------------------

# Start time

# rf.start_time <- proc.time()[3]
# 
# # Training Random Forest 
# 
# model_RF <- randomForest(visit~., oversampled.trainingdata)
# 
# # Stop time 
# 
# rf.stop_time <- proc.time()[3]
# 
# # Calculate training time
# 
# rf.time_elapsed <- rf.stop_time - rf.start_time

# rf.time_elapsed <- round(rf.time_elapsed, 2)

# print(paste("Random Forest model took", rf.time_elapsed, "seconds to train."))

# Making the predictions

# RF_pred <- predict(model_RF, testdata)

# Confusion Matrix 

# cat("Random Forest Confusion Matrix\n--------------------------------------\n")

#(rf.confusion_matrix <- confusionMatrix(RF_pred, testdata$visit, positive='1', mode = "prec_recall"))

# print(model_RF)

# Check the important attributes

# varImpRf <- varImp(model_RF)
# varImpRf$Overall <- sort(varImpRf$Overall, decreasing = T)
varImpRf

# Check the output for visit probabilities

# RF_prob <- predict(model_RF,testdata , type = "prob") 

# accuracy.df[2,] <- c("Random Forest", rf.time_elapsed,
                     round(rf.confusion_matrix$overall["Accuracy"]*100,2),
                     round(rf.confusion_matrix$byClass["F1"],2),
                     round(rf.confusion_matrix$byClass["Precision"]*100,2),
                          round(rf.confusion_matrix$byClass["Recall"]*100,2),
                          round(rf.confusion_matrix$byClass["Sensitivity"]*100,2),
                     round(rf.confusion_matrix$byClass["Specificity"]*100,2))

#--------------------------
# XGBoost
#--------------------------


# Creating test and train variables in xgboost input format

train.X <- sparse.model.matrix(visit ~., data = oversampled.trainingdata)
train.y <- as.numeric(as.character(oversampled.trainingdata$visit))


test.X <- sparse.model.matrix(visit ~., data = testdata)
test.y <- as.numeric(as.character(testdata$visit))

#creating xboost matrix

train.xgb.matrix <- xgb.DMatrix(data = as.matrix(train.X), label = train.y)

# Setting trainControl for Cross Validation 

train_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE, allowParallel = TRUE)

tune_grid <- expand.grid(
  nrounds = 200,
  eta = 0.05,
  max_depth = 6,
  gamma = 0,
  min_child_weight = 3,
  colsample_bytree = 0.6,
  subsample =  1
)

# Start time

xgb.start_time <- proc.time()[3]

# Training the model

xgb_model <- train(
  x = train.X,
  y = oversampled.trainingdata$visit,
  trControl = train_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE,
  verbosity = 0
)

# Stop time

xgb.stop_time <- proc.time()[3]

# Training time

xgb.elapsed <- xgb.stop_time - xgb.start_time

xgb.elapsed <- round(xgb.elapsed, 2)

print(paste("XGBoost model took", xgb.elapsed, "seconds to train."))

# Making the predictions

xgb.predictions <- predict(xgb_model, test.X)
xgb_prob <- predict(xgb_model, test.X, type = "prob")

cat("XGBoost Confusion Matrix\n--------------------------------------\n")
(xgb_model.confusion_matrix <- confusionMatrix(table(xgb.predictions, test.y)))


# Feature Importance 

varImp(xgb_model)


accuracy.df[1,] <- c("XGBoost", xgb.elapsed,
                     round(xgb_model.confusion_matrix$overall["Accuracy"]*100,2),
                     round(xgb_model.confusion_matrix$byClass["F1"],2),
                     round(xgb_model.confusion_matrix$byClass["Precision"]*100,2),
                          round(xgb_model.confusion_matrix$byClass["Recall"]*100,2),
                          round(xgb_model.confusion_matrix$byClass["Sensitivity"]*100,2),
                     round(xgb_model.confusion_matrix$byClass["Specificity"]*100,2)
                          )

#--------------------------
# Logistic Regression
#--------------------------

# Start time

# LogR.start_time <- proc.time()[3]
# 
# # Training the model 
# 
# LogR_model <- glm(visit~.  , data = oversampled.trainingdata, family = "binomial")
# 
# # Stop time
# 
# LogR.stop_time <- proc.time()[3]
# 
# # Trainging Time
# 
# LogR.time_elapsed <- LogR.stop_time - LogR.start_time

# LogR.time_elapsed <- round(LogR.time_elapsed, 2)

# print(paste("Logistic Regression model took", LogR.time_elapsed, "seconds to train."))

# Making the predictions

# LogR_pred <- predict(LogR_model, testdata, type="response")
# 
# # Predict the visit outcome 
# 
# LogR_visit <- ifelse(LogR_pred > 0.5, 1, 0)
# 
# # Save the predictions as factor variables
# 
# LogR_visit <- as.factor(LogR_visit)

# cat("Logistic Regression Confusion Matrix\n--------------------------------------\n")

# (LogR.confusion_matrix <- confusionMatrix(LogR_visit, testdata$visit, positive='1', mode = "prec_recall"))

# accuracy.df[4,] <- c("Logistic Regression", LogR.time_elapsed,
                     round(LogR.confusion_matrix$overall["Accuracy"]*100,2),
                     round(LogR.confusion_matrix$byClass["F1"],2),
                     round(LogR.confusion_matrix$byClass["Precision"]*100,2),
                          round(LogR.confusion_matrix$byClass["Recall"]*100,2),
                          round(LogR.confusion_matrix$byClass["Sensitivity"]*100,2),
                     round(LogR.confusion_matrix$byClass["Specificity"]*100,2)
                          )

#--------------------------
# SVM
#--------------------------

# Start time 

# svm.start_time <- proc.time()[3]
#
# svm_model <- svm(visit~., data = oversampled.trainingdata, kernel = "radial", scale = T, cross = 5, probability = T)
#
# # Stop Time
#
# svm.stop_time <- proc.time()[3]
# 
# svm.time_elapsed <- svm.stop_time - svm.start_time
# 
# svm.time_elapsed <- round(svm.time_elapsed, 2)

# print(paste("SVM model took", svm.time_elapsed, "seconds to train."))

# svm_predict <- predict(svm_model, testdata)

# Use confusionMatrix to print the performance of SVM model

 #cat("SVM Confusion Matrix\n--------------------------------------\n")

# (svm.confusion_matrix <- confusionMatrix(svm_predict, testdata$visit, 
                                         positive = "1", mode = "prec_recall"))

# SVM_prob <- predict(svm_model, testdata, probability = T)
# SVM_probability <- attr(SVM_prob, "probabilities") 

# accuracy.df[5,] <- c("SVM", svm.time_elapsed,
                     round(svm.confusion_matrix$overall["Accuracy"]*100,2),
                     round(svm.confusion_matrix$byClass["F1"],2),
                     round(svm.confusion_matrix$byClass["Precision"]*100,2),
                     round(svm.confusion_matrix$byClass["Recall"]*100,2),
                     round(svm.confusion_matrix$byClass["Sensitivity"]*100,2),
                     round(svm.confusion_matrix$byClass["Specificity"]*100,2)
                     )

# Display model metrics 

accuracy.df %>% kbl(caption = "Model Evaluation Metrics") %>% kable_styling(full_width = F)


#--------------------------
# ROC
#--------------------------


# Logistic Regression
# ROC_LogReg <- roc(testdata$visit, LogR_pred)

# Random Forest
# ROC_RF <- roc(testdata$visit, RF_prob[,2])

# SVM
# ROC_SVM <- roc(testdata$visit,SVM_probability[,2])

# XGBoost
ROC_XGB <- roc(testdata$visit, xgb_prob[,2])

# Plot the ROC curve for all models
# ggroc(list("Logistic Regression" = ROC_LogReg, "SVM" = ROC_SVM, "Random Forest" = ROC_RF, "XGBoost" = ROC_XGB), legacy.axes=TRUE)+ xlab("False Positive Rate") + ylab("True Positive Rate") + geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype = "dashed") + scale_colour_manual(values = c("pink", "green", "blue", "red")) + labs(color='Model Name')

ggroc(list("XGBoost" = ROC_XGB), legacy.axes=TRUE)+ xlab("False Positive Rate") + ylab("True Positive Rate") + geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype = "dashed") 

#--------------------------
# AUC Scores
#--------------------------


#Calculate the area under the curve (AUC) for Logistic Regression
# AUC_LogReg <- auc(ROC_LogReg)

#Calculate the area under the curve (AUC) for SVM
# AUC_SVM <- auc(ROC_SVM)

#Calculate the area under the curve (AUC) for Random Forest
# AUC_RF <- auc(ROC_RF)

#Calculate the area under the curve (AUC) for XGBoost
AUC_XGB <- auc(ROC_XGB)

#Calculate the area under the curve (AUC) for Decision Tree
# AUC_DT <- auc(ROC_DT)

auc.df <- data.frame(Model = character(), AUC = numeric())
invisible(rbind(auc.df, list(Model = NA, AUC = NA)))

# auc.df[1, ] <- c("Decision Tree", round(as.numeric(AUC_DT),2))
auc.df[1, ] <- c("XGBoost", round(as.numeric(AUC_XGB),2))
# auc.df[2, ] <- c("Random Forest", round(as.numeric(AUC_RF),2))
# auc.df[4, ] <- c("Logistic Regression", round(as.numeric(AUC_LogReg),2))
# auc.df[5, ] <- c("SVM", round(as.numeric(AUC_SVM),2))

auc.df %>% kbl(caption = "AUC of Models") %>% kable_styling(full_width = T)

#--------------------------
# Cumulative Gains
#--------------------------

# Obtain cumulative gains table for Logistic Regression
# GainTable_LogReg <- cumGainsTable(LogR_pred, testdata$visit, resolution = 1/100)

# Obtain cumulative gains table for SVM
# GainTable_SVM <- cumGainsTable(as.numeric(SVM_prob), testdata$visit, resolution = 1/100)

# Obtain cumulative gains table for Random Forest
# GainTable_RF <- cumGainsTable(RF_prob[,2], testdata$visit, resolution = 1/100)

# Obtain cumulative gains table for Random Forest
# GainTable_DT <- cumGainsTable(as.numeric(predict_tree_prune),testdata$visit, resolution = 1/100)
 
# Obtain cumulative gains table for Random Forest
# GainTable_XGB <- cumGainsTable(xgb_prob[,2], testdata$visit, resolution = 1/100)

#Plotting the cumulative gains 

plot(GainTable_LogReg[,4], col="lightseagreen", type="l",
xlab="Percentage of test instances", ylab="Percentage of correct predictions")
lines(GainTable_SVM[,4], col="green", type ="l")
lines(GainTable_RF[,4], col="blue", type ="l")
lines(GainTable_DT[,4], col="purple", type ="l")
lines(GainTable_XGB[,4], col="red", type ="l")
grid(NULL, lwd = 1)

legend("bottomright",
c("LogReg", "SVM", "Random Forest", "Decision Tree", "XGBoost"),
fill=c("lightseagreen","green", "blue", "purple", "red"))



