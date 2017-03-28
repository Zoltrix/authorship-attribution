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

#set the main seed
set.seed(12345)

#set seeds to be used by model training
seeds <- vector(mode = "list", length = 4)
for(i in 1:3) seeds[[i]]<- sample.int(n=1000, 1)
seeds[[4]]<-sample.int(1000, 1)


train_model <- function(df, seeds, method) {
  train_control<- trainControl(method="cv", seeds = seeds, number=3, savePredictions = TRUE)
  mdl <- train(y ~ ., data = df, trControl = train_control, method = method)
  
  mdl$pred
}

users <- read_data("data/sample/users/")
stop_words <- read_lines("data/stopwords.txt")

users_with_textual_features <- add_textual_features(users)
preprocess <- process_data(users_with_textual_features, stop_words)

registerDoMC(cores = 4)
pred <- train_model(preprocess$processed_df, seeds, 'svmLinear')
