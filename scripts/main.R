setwd("Projects/authorship-attribution/")

# LIBRARIES ----
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(caret)
library(doMC)
library(tm)

# SOURCE FILES ----
source("scripts/read_data.R")

users <- read_data("data/sample/users/")

createOneVsRest <- function(df, name, original_df) {
  name <- name[1]
  rest <- original_df %>% 
    filter(uname != name)
  
  rest_all <- do.call("rbind", lapply(unique(rest$uname), function(usr){
    d <- rest[rest$uname == usr, ]
    d %>% 
      sample_n( min(nrow(d), nrow(df)) )
  }))
  
  df$y <- 1
  rest_all$y <- 0
  
  bind_rows(df, rest_all)
}


knn_model <- function(df) {
  corpus <- Corpus(VectorSource(df$text))
  
  tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, 
                                         #stopwords = TRUE, 
                                         #stemming = TRUE, 
                                         removeNumbers = TRUE, 
                                         bounds = list(local = c(2, Inf))))
  
  train <- as.matrix(tdm)
  train <- as.data.frame(train)
  train$y <- as.factor(df$y)
  
  registerDoMC(cores = 4)
  
  train_control<- trainControl(method="cv", number=3, savePredictions = TRUE)
  mdl <- train(y ~ ., data = train, trControl = train_control, method = 'ranger')
  
  mdl$pred
}

users_ovr <- users %>% 
  group_by(uname) %>% 
  nest() %>% 
  mutate(
    oneVsRest = map(data, createOneVsRest, uname, users)
  )

users_ovr %>% 
  mutate(knn_mdl = map(oneVsRest, knn_model))

users$y <- users$uname
users$uname <- NULL

knn_model(users)
