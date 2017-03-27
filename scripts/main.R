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


knn_model <- function(df, stop_words) {
  corpus <- VCorpus(VectorSource(df$text))
  
  corpus <- tm_map(corpus, removeWords, stop_words) 
  
  tdm <- DocumentTermMatrix(corpus, control = list(removePunctuation = TRUE, 
                                         #stopwords = TRUE, 
                                         #stemming = TRUE, 
                                         removeNumbers = TRUE, 
                                         weighting = weightTfIdf,
                                         bounds = list(global = c(5, Inf))))
  
  BigramTokenizer <-
    function(x)
      unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  #library(RWeka)
  #BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  
  bigrams_tdm <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer, 
                                                   removeNumbers = TRUE, 
                                                   weighting = weightTfIdf,
                                                   bounds = list(global = c(2, Inf))))

  train_words <- as.data.frame(as.matrix(tdm))
  train_bigrams <- as.data.frame(as.matrix(bigrams_tdm))
  
  train <- bind_cols(train_words, train_bigrams)
  train$y <- as.factor(df$y)
  
  train <- train %>% 
    mutate(row_sums = rowSums( .[1: (ncol(train)-1)] ) ) %>% 
    filter(row_sums > 0)
  
  registerDoMC(cores = 4)
  
  train_control<- trainControl(method="cv", number=3, savePredictions = TRUE)
  mdl <- train(y ~ ., data = train, trControl = train_control, method = 'svmLinear')
  
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

stop_words <- read_lines("data/stopwords.txt")

knn_model(users, stop_words)
