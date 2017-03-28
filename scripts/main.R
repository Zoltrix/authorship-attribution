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

users <- read_data("data/sample/users/")

train_model <- function(df, stop_words, method) {
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
  mdl <- train(y ~ ., data = train, trControl = train_control, method = method)
  
  mdl$pred
}

user_tokens <- users %>% 
  unnest_tokens(word, text, token = stringr::str_split, pattern = " ") %>% 
  group_by(uname, file_id) %>%
  mutate(
    new_lines = str_count(word, "\\\\n"),
    special_characters = str_count(word, "[!\\:\\?]"),
    underscores = str_count(word, "_")
    #TODO- emojis
    
  ) %>% ungroup()

user_tokens.agg <- user_tokens %>% 
  group_by(uname, file_id) %>% 
  summarise(
    newlines = sum(new_lines),
    special_chars = sum(special_characters),
    underscores = sum(underscores)
  )

users <- users %>% 
  inner_join(user_tokens.agg)

users$y <- users$uname
users$uname <- NULL

stop_words <- read_lines("data/stopwords.txt")

pred <- train_model(users, stop_words, 'svmLinear')
