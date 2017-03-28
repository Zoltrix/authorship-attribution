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

users <- read_data("data/sample/users/")

train_model <- function(df, method) {
  train_control<- trainControl(method="cv", number=3, savePredictions = TRUE)
  mdl <- train(y ~ ., data = df, trControl = train_control, method = method)
  
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

stop_words <- read_lines("data/stopwords.txt")

registerDoMC(cores = 4)
preprcess <- process_data(users, stop_words)

pred <- train_model(preprcess$processed_df, 'svmLinear')
