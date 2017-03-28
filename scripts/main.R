setwd("Projects/authorship-attribution/")

## 1) Continue with investigating the distribution of the newly added features
## we might need to scale them according to the algorithm

## 2) Do not use aggregation ... use per document calculations instead
# LIBRARIES ----
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(caret)
library(doMC)
library(tidytext)
library(stringr)
library(tm)

# SOURCE FILES ----
source("scripts/read_data.R")
source("scripts/split_data.R")
source("scripts/process.R")
source("scripts/textual_features.R")

# SET SEED ----
#set the main seed
set.seed(12345)

#set seeds to be used by model training
seeds <- vector(mode = "list", length = 4)
for(i in 1:3) seeds[[i]]<- sample.int(n=1000, 1)
seeds[[4]]<-sample.int(1000, 1)

# MODEL TRAINING ----
train_model <- function(df, seeds, method) {
  train_control<- trainControl(method="cv", seeds = seeds, number=3, savePredictions = TRUE)
  fit <- train(y ~ ., data = df, trControl = train_control, method = method)
  
  fit
}

model_performance <- function(fit) {
  confustion_mtrx <- confusionMatrix(data = mdl$pred$pred, reference = mdl$pred$obs, mode = "prec_recall")

  cat("Accuracy: ", confustion_mtrx$overall["Accuracy"], "\n")
  cat("Precision: ", mean(confustion_mtrx$byClass[, "Precision"]), "\n")
  cat("Recall: ", mean(confustion_mtrx$byClass[, "Recall"]), "\n")
} 

users <- read_data("data/sample/users/")
stop_words <- read_lines("data/stopwords.txt")

# DATA SPLIT ----
data_split <- split_data(users)
trData <- data_split$training
tstData <- data_split$testing


users_with_textual_features <- add_textual_features(trData)
preprocess <- process_data(users_with_textual_features, stop_words)

registerDoMC(cores = 4)
mdl <- train_model(preprocess$processed_df, seeds, 'svmLinear')

model_performance(mdl)
