#-------------------------------------------------------------------------------
# Predicting what hotel a user will book based on some attributes about the 
# search the user is conducting on Expedia.
# Training Dataset : 24,154x41
# Testing Dataset  : 14,986x41
# Target Variable  : hotel_cluster
# ML Method        : xgboost with 5 fold cross-validation
# Evaluation       : Mean Average Precision @ 5
#-------------------------------------------------------------------------------

# Environment Setup
rm(list=ls(all=TRUE))
setwd("E:/Education/Kaggle/Expedia_Hotel_Recommendations")

# Load libraries
library(data.table)
library(caret)
library(xgboost)

# Load dataset
load(file="Inputs/preparedDatasets.RData")
trainData[, hotel_cluster:=hotel_cluster-1]
testData[, hotel_cluster:=hotel_cluster-1]

# Split training dataset into training and validation datasets
set.seed(1718)
trainIndex <- sample(nrow(trainData), round(nrow(trainData)*0.9))
train_dt <- trainData[ trainIndex, ]
valid_dt <- trainData[-trainIndex, ]

# Convert data.table into data.frame
train_df <- data.frame(train_dt)
valid_df <- data.frame(valid_dt)
test_df <- data.frame(testData)

# Remove unwanted data from the session
rm(list=setdiff(ls(), c("train_df", "valid_df", "test_df")))
gc()

# Features and outcome
outcome_name <- "hotel_cluster"
feature_names <- setdiff(names(train_df), c(outcome_name, "date_time", "srch_ci", "srch_co"))


#-------------------------------------------------------------------------------
# Evaluation Metrics
#-------------------------------------------------------------------------------
# Function to evaluate the model
#' Compute the mean average precision at k
#'
#' This function computes the mean average precision at k
#' of two lists of sequences.
#'
#' @param k max length of predicted sequence
#' @param actual list of ground truth sets (vectors)
#' @param predicted list of predicted sequences (vectors)
#' @export
apk <- function(k, actual, predicted) {
  score <- 0.0
  cnt <- 0.0
  for(i in 1:min(k, length(predicted))) {
    if(predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)])) {
      cnt <- cnt + 1
      score <- score + cnt/i 
    }
  }
  score <- score / min(length(actual), k)
  score
}

mapk <- function(k, actual, predicted) {
  if(length(actual)==0 || nrow(predicted)==0) {
    return(0.0)
  }
  scores <- rep(0, length(actual))
  for (i in 1:length(scores)) {
    scores[i] <- apk(k, actual[i], predicted[i, ])
  }
  score <- mean(scores)
  score
}

#-------------------------------------------------------------------------------
# Model Development
#-------------------------------------------------------------------------------

# Set tuning parameters
param0 <- list(booster = "gbtree"
  , eta = 0.3
  , max_depth = 6
  , min_child_weight = 1
  , subsample = 1
  , colsample_bytree = 1
  , objective = "multi:softprob"
  # , eval_metric = mapk
  , num_class = 14
)

# CV
dtrain <- xgb.DMatrix(data=data.matrix(train_df[, feature_names]), label=train_df[, outcome_name], missing=NA)
dtest  <- xgb.DMatrix(data=data.matrix(test_df[,  feature_names]), label=test_df [, outcome_name], missing=NA)

set.seed(1718)
cv_model <- xgb.cv(params = param0, data=dtrain, nrounds=1000, nfold=3)
cv_model[, N:=1:.N]
new_nrounds <- cv_model[which.min(test.merror.mean), N]

set.seed(1718)
xgb_model <- xgb.train(params = param0, data=dtrain, nrounds=new_nrounds)
tmpPred <- predict(xgb_model, dtest)
test_pred <- matrix(tmpPred, ncol=14, byrow=TRUE)

k <- 5
pred_order <- apply(test_pred, 1, function(x) order(x, decreasing=TRUE)[1:k]-1)
actual <- test_df [, outcome_name]
predicted <- t(pred_order)
final_precision <- mapk(k, actual, predicted)
final_precision # 0.7046688

variableImp <- xgb.importance(feature_names=feature_names, model=xgb_model)


#-------------------------------------------------------------------------------
# Modify Outputs for Presentation
#-------------------------------------------------------------------------------
train_dt <- data.table(train_df)
train_dt[, hotel_cluster:=as.character(hotel_cluster)]
train_dt[hotel_cluster=="0", hotel_cluster:="Hilton Hotels & Resorts"]
train_dt[hotel_cluster=="1", hotel_cluster:="Waldorf Astoria Hotels & Resorts"]
train_dt[hotel_cluster=="2", hotel_cluster:="Conrad Hotels & Resorts"]
train_dt[hotel_cluster=="3", hotel_cluster:="Canopy by Hilton"]
train_dt[hotel_cluster=="4", hotel_cluster:="Curio – A Collection by Hilton"]
train_dt[hotel_cluster=="5", hotel_cluster:="Doubletree by Hilton"]
train_dt[hotel_cluster=="6", hotel_cluster:="Tapestry Collection by Hilton"]
train_dt[hotel_cluster=="7", hotel_cluster:="Embassy Suites by Hilton"]
train_dt[hotel_cluster=="8", hotel_cluster:="Hilton Garden Inn"]
train_dt[hotel_cluster=="9", hotel_cluster:="Hampton by Hilton"]
train_dt[hotel_cluster=="10", hotel_cluster:="Tru by Hilton"]
train_dt[hotel_cluster=="11", hotel_cluster:="Homewood Suites by Hilton"]
train_dt[hotel_cluster=="12", hotel_cluster:="Home2 Suites by Hilton"]
train_dt[hotel_cluster=="13", hotel_cluster:="Hilton Grand Vacations"]

test_dt <- data.table(test_df)
test_dt[, hotel_cluster:=as.character(hotel_cluster)]
test_dt[hotel_cluster=="0", hotel_cluster:="Hilton Hotels & Resorts"]
test_dt[hotel_cluster=="1", hotel_cluster:="Waldorf Astoria Hotels & Resorts"]
test_dt[hotel_cluster=="2", hotel_cluster:="Conrad Hotels & Resorts"]
test_dt[hotel_cluster=="3", hotel_cluster:="Canopy by Hilton"]
test_dt[hotel_cluster=="4", hotel_cluster:="Curio – A Collection by Hilton"]
test_dt[hotel_cluster=="5", hotel_cluster:="Doubletree by Hilton"]
test_dt[hotel_cluster=="6", hotel_cluster:="Tapestry Collection by Hilton"]
test_dt[hotel_cluster=="7", hotel_cluster:="Embassy Suites by Hilton"]
test_dt[hotel_cluster=="8", hotel_cluster:="Hilton Garden Inn"]
test_dt[hotel_cluster=="9", hotel_cluster:="Hampton by Hilton"]
test_dt[hotel_cluster=="10", hotel_cluster:="Tru by Hilton"]
test_dt[hotel_cluster=="11", hotel_cluster:="Homewood Suites by Hilton"]
test_dt[hotel_cluster=="12", hotel_cluster:="Home2 Suites by Hilton"]
test_dt[hotel_cluster=="13", hotel_cluster:="Hilton Grand Vacations"]


hotels <- c("Hilton Hotels & Resorts", "Waldorf Astoria Hotels & Resorts", "Conrad Hotels & Resorts",
  "Canopy by Hilton", "Curio – A Collection by Hilton", "Doubletree by Hilton", "Tapestry Collection by Hilton",
  "Embassy Suites by Hilton", "Hilton Garden Inn", "Hampton by Hilton", "Tru by Hilton", "Homewood Suites by Hilton",
  "Home2 Suites by Hilton", "Hilton Grand Vacations")

pred_out_pref <- apply(test_pred, 1, function(x) hotels[order(x, decreasing=TRUE)])
pred_out_pref <- t(pred_out_pref)
pred_out_pref <- data.table(pred_out_pref)
setnames(pred_out_pref, paste0("Preference_", 1:14))

pred_out_prob <- data.table(test_pred)
setnames(pred_out_prob, hotels)

#-------------------------------------------------------------------------------
# Save outputs
#-------------------------------------------------------------------------------
xgb.save(xgb_model, "Solutions/Outputs/xgboost.model")
write.csv(variableImp, "Solutions/Outputs/varImp.csv", row.names=FALSE)
write.csv(train_dt, "Solutions/Outputs/trainingDataset.csv", row.names=FALSE)
write.csv(test_dt, "Solutions/Outputs/testingDataset.csv", row.names=FALSE)
write.csv(pred_out_prob, "Solutions/Outputs/test_prediction_probabilities.csv", row.names=FALSE)
write.csv(pred_out_pref, "Solutions/Outputs/test_prediction_Preference.csv", row.names=FALSE)

# png("Solutions/Outputs/variableImpPlot.png", width=800, height=600)
xgb.plot.importance(importance_matrix = variableImp, numberOfClusters = c(1:10))
# dev.off()

xgb.plot.tree(feature_names = feature_names, model = xgb_model, n_first_tree = 1)
save.image("Solutions/Outputs/Session_image.RData")
