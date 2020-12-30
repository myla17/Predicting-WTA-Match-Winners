# Import and view data
WTA.Match.Data <- read.csv("~/Desktop/4740 Project/WTA/WTA Match Data.csv")
View(WTA.Match.Data)

#Analyze data first

#load library needed for partitioning our data and logistic regression
library(caret)
library(MASS)

#I consistently use this seed value throughout the model to guarantee results are reproducible 
set.seed(143)

#The following commands partition data into a 40/60 training/scoring split
trainIndex <- createDataPartition(WTA.Match.Data$won_match, p = .4, list = FALSE, times = 1)
matchTrain <- WTA.Match.Data[ trainIndex,]
matchScore <- WTA.Match.Data[-trainIndex,]

#get summary of training data
summary(matchTrain)

#View an identify any variables closely related to our predictor: won_match
#First, i create a separate data frame of my numeric columns only
library(dplyr)
numericCols <- matchTrain %>% select(1, 9, 13, 15, 19:30)
cors <- data.frame(cor(na.omit(numericCols))
View(cors)

# set seed again
set.seed(143)

attach(matchTrain)
# Note: all model processes remove missing variables 
#run model on training data using selected variables (based upon player attributes and match stats)
WTAMatchModel_part <-glm(as.factor(won_match) ~ player_ht + player_age + minutes + player_aces + player_df + player_svpt + player_1stIn + player_first_serve_points + player_second_serve_points + player_serv_games + player_bpSaved + player_bpFaced + player_rank + player_rank_points, data = na.omit(matchTrain), family = binomial())

#Generate predictions based on model
Prediction_Set2 <- predict(WTAMatchModel_part, na.omit(matchScore), type = "response")

#Output prediction set
Output2 <- data.frame(Prediction_Set2, na.omit(matchScore))
#View model
WTAMatchModel_part

#Load library necessary for cross-validation
library(tidyverse)
set.seed(143)

#using 10 k-folds analyze and output the true accuracy of the logistic regression
matchTrainControl <- trainControl(method = "cv", number = 10)
WTAMatchModel_part <-train(as.factor(won_match) ~ player_ht + player_age + minutes + player_aces + player_df + player_svpt + player_1stIn + player_first_serve_points + player_second_serve_points + player_serv_games + player_bpSaved + player_bpFaced + player_rank + player_rank_points, data = na.omit(matchTrain), method = "glm", trControl = matchTrainControl, family = binomial())
print(WTAMatchModel_part)




