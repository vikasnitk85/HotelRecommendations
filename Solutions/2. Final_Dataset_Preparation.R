#-------------------------------------------------------------------------------
# Predicting what hotel a user will book based on some attributes about the 
# search the user is conducting on Expedia.
#-------------------------------------------------------------------------------

# Environment Setup
rm(list=ls(all=TRUE))
setwd("E:/Education/Kaggle/Expedia_Hotel_Recommendations")
library(data.table)

# Import Data
trainData <- fread("Inputs/sample_train.csv")
testData <- fread("Inputs/sample_test.csv")
destinations <- fread("Inputs/destinations.csv")
destination_id <- destinations[, srch_destination_id]
destinations[, srch_destination_id:=NULL]

# Generate features from destinations
pca <- prcomp(destinations)
summary(pca)
plot(pca, type = "l")
dest_small <- predict(pca, newdata=destinations)
dest_small <- dest_small[, 1:3]
dest_small <- data.table(dest_small)
dest_small[, srch_destination_id:=destination_id]
setcolorder(dest_small, c("srch_destination_id", "PC1", "PC2", "PC3"))

#-------------------------------------------------------------------------------
# Generate features based on dates
# Training dataset
#-------------------------------------------------------------------------------
trainData[, date_time:=as.Date(date_time, "%Y-%m-%d")]
trainData[, srch_ci:=as.Date(srch_ci, "%Y-%m-%d")]
trainData[, srch_co:=as.Date(srch_co, "%Y-%m-%d")]

trainData[, date_year:=NULL]
setnames(trainData, "date_month", "date_time_month")
trainData[, date_time_mday:=mday(date_time)]
trainData[, date_time_wday:=wday(date_time)]
trainData[, date_time_quarter:=quarter(date_time)]

trainData[, srch_ci_month:=month(srch_ci)]
trainData[, srch_ci_mday:=mday(srch_ci)]
trainData[, srch_ci_wday:=wday(srch_ci)]
trainData[, srch_ci_quarter:=quarter(srch_ci)]

trainData[, srch_co_month:=month(srch_co)]
trainData[, srch_co_mday:=mday(srch_co)]
trainData[, srch_co_wday:=wday(srch_co)]
trainData[, srch_co_quarter:=quarter(srch_co)]

trainData[, stay_span:=as.numeric(srch_co - srch_ci)]
trainData[, trip_span:=as.numeric(srch_ci - date_time)]
trainData[, srch_family_cnt:=srch_adults_cnt+srch_children_cnt]

trainData <- merge(trainData, dest_small, by="srch_destination_id")
setcolorder(trainData, c("user_id", "srch_destination_id", "date_time", "srch_ci", "srch_co",
  "site_name", "posa_continent", "user_location_country", "user_location_region", "user_location_city",
  "orig_destination_distance", "is_mobile", "is_package", "channel", "srch_adults_cnt", "srch_children_cnt",
  "srch_family_cnt", "srch_rm_cnt", "srch_destination_type_id", "is_booking", "cnt", "hotel_continent",
  "hotel_country", "hotel_market", "date_time_month", "date_time_mday", "date_time_wday", "date_time_quarter", 
  "srch_ci_month", "srch_ci_mday", "srch_ci_wday", "srch_ci_quarter", "srch_co_month", "srch_co_mday",
  "srch_co_wday", "srch_co_quarter", "stay_span", "trip_span", "PC1", "PC2", "PC3", "hotel_cluster")
)
trainData[hotel_cluster== 7, hotel_cluster:=1]
trainData[hotel_cluster==12, hotel_cluster:=2]
trainData[hotel_cluster==23, hotel_cluster:=3]
trainData[hotel_cluster==31, hotel_cluster:=4]
trainData[hotel_cluster==38, hotel_cluster:=5]
trainData[hotel_cluster==43, hotel_cluster:=6]
trainData[hotel_cluster==45, hotel_cluster:=7]
trainData[hotel_cluster==49, hotel_cluster:=8]
trainData[hotel_cluster==54, hotel_cluster:=9]
trainData[hotel_cluster==66, hotel_cluster:=10]
trainData[hotel_cluster==67, hotel_cluster:=11]
trainData[hotel_cluster==87, hotel_cluster:=12]
trainData[hotel_cluster==89, hotel_cluster:=13]
trainData[hotel_cluster==92, hotel_cluster:=14]

#-------------------------------------------------------------------------------
# Generate features based on dates
# Testing dataset
#-------------------------------------------------------------------------------
testData[, date_time:=as.Date(date_time, "%Y-%m-%d")]
testData[, srch_ci:=as.Date(srch_ci, "%Y-%m-%d")]
testData[, srch_co:=as.Date(srch_co, "%Y-%m-%d")]

testData[, date_year:=NULL]
setnames(testData, "date_month", "date_time_month")
testData[, date_time_mday:=mday(date_time)]
testData[, date_time_wday:=wday(date_time)]
testData[, date_time_quarter:=quarter(date_time)]

testData[, srch_ci_month:=month(srch_ci)]
testData[, srch_ci_mday:=mday(srch_ci)]
testData[, srch_ci_wday:=wday(srch_ci)]
testData[, srch_ci_quarter:=quarter(srch_ci)]

testData[, srch_co_month:=month(srch_co)]
testData[, srch_co_mday:=mday(srch_co)]
testData[, srch_co_wday:=wday(srch_co)]
testData[, srch_co_quarter:=quarter(srch_co)]

testData[, stay_span:=as.numeric(srch_co - srch_ci)]
testData[, trip_span:=as.numeric(srch_ci - date_time)]
testData[, srch_family_cnt:=srch_adults_cnt+srch_children_cnt]
testData <- merge(testData, dest_small, by="srch_destination_id")
setcolorder(testData, c("user_id", "srch_destination_id", "date_time", "srch_ci", "srch_co",
  "site_name", "posa_continent", "user_location_country", "user_location_region", "user_location_city",
  "orig_destination_distance", "is_mobile", "is_package", "channel", "srch_adults_cnt", "srch_children_cnt",
  "srch_family_cnt", "srch_rm_cnt", "srch_destination_type_id", "is_booking", "cnt", "hotel_continent",
  "hotel_country", "hotel_market", "date_time_month", "date_time_mday", "date_time_wday", "date_time_quarter", 
  "srch_ci_month", "srch_ci_mday", "srch_ci_wday", "srch_ci_quarter", "srch_co_month", "srch_co_mday",
  "srch_co_wday", "srch_co_quarter", "stay_span", "trip_span", "PC1", "PC2", "PC3", "hotel_cluster")
)
testData[hotel_cluster== 7, hotel_cluster:=1]
testData[hotel_cluster==12, hotel_cluster:=2]
testData[hotel_cluster==23, hotel_cluster:=3]
testData[hotel_cluster==31, hotel_cluster:=4]
testData[hotel_cluster==38, hotel_cluster:=5]
testData[hotel_cluster==43, hotel_cluster:=6]
testData[hotel_cluster==45, hotel_cluster:=7]
testData[hotel_cluster==49, hotel_cluster:=8]
testData[hotel_cluster==54, hotel_cluster:=9]
testData[hotel_cluster==66, hotel_cluster:=10]
testData[hotel_cluster==67, hotel_cluster:=11]
testData[hotel_cluster==87, hotel_cluster:=12]
testData[hotel_cluster==89, hotel_cluster:=13]
testData[hotel_cluster==92, hotel_cluster:=14]

#-------------------------------------------------------------------------------
# Save prepared datasets
#-------------------------------------------------------------------------------
save(trainData, testData, file="Inputs/preparedDatasets.RData")
