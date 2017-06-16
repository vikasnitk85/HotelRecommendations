#-------------------------------------------------------------------------------
# Predicting what hotel a user will book based on some attributes about the 
# search the user is conducting on Expedia.
#-------------------------------------------------------------------------------

# Environment Setup
rm(list=ls(all=TRUE))
setwd("E:/Education/Kaggle/Expedia_Hotel_Recommendations")
library(data.table)

# Import Data
rawData <- fread("Inputs/train.csv")

# Dimension of the Data
dim(rawData) # 37670293x24

# Explore Hotel Clusters
rawData[, .N, by="hotel_cluster"]
hotelCluster <- rawData[, .N, by="hotel_cluster"]
hotelCluster[, prop:=N/sum(N)]
setorder(hotelCluster, -N)
newHotelCluster <- hotelCluster[prop > 0.006 & prop < 0.0073, ]

# Add in times and dates
rawData[, date_time:=as.Date(date_time, "%Y-%m-%d")]
rawData[, date_year:=year(date_time)]
rawData[, date_month:=month(date_time)]
gc()

# Filter data based on selected hotel cluster
clusters <- newHotelCluster[, hotel_cluster]
filteredData <- rawData[hotel_cluster %in% clusters, ]
gc()

# Again filter the data based on user id
set.seed(1234)
unique_users <- unique(filteredData[, user_id])
sel_user_ids <- sample(unique_users, 15000)
sel_train <- filteredData[user_id %in% sel_user_ids, ]

# Split sel_train into training and testing datasets
DT1 <- sel_train[date_time <= as.Date("2014-07-31"), ]
DT2  <- sel_train[date_time >  as.Date("2014-07-31"), ]

# Remove user_ids, which are not common in both the data
uniqueUser <- intersect(DT1[, unique(user_id)], DT2[, unique(user_id)])
trainingData <- DT1[user_id %in% uniqueUser, ]   # 24,223 (61.6%)
testingData  <- DT2[user_id %in% uniqueUser, ]   # 15,097 (38.4%)

write.csv(trainingData, "Inputs/sample_train.csv", row.names=FALSE)
write.csv(testingData, "Inputs/sample_test.csv", row.names=FALSE)
