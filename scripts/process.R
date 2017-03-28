process_data <- function(df, stop_words, min_word_threshold = 5, min_bigram_threshold = 2) {
  corpus <- VCorpus(VectorSource(df$text))
  
  corpus <- tm_map(corpus, removeWords, stop_words) 
  
  dtm <- DocumentTermMatrix(corpus, control = list(removePunctuation = TRUE, 
                                                   #stopwords = TRUE, 
                                                   #stemming = TRUE, 
                                                   removeNumbers = TRUE, 
                                                   weighting = weightTfIdf,
                                                   bounds = list(global = c(min_word_threshold, Inf))))
  
  BigramTokenizer <-
    function(x)
      unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
  
  bigrams_dtm <- DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer, 
                                                           removeNumbers = TRUE, 
                                                           weighting = weightTfIdf,
                                                           bounds = list(global = c(min_bigram_threshold, Inf))))
  
  train_words <- as.data.frame(as.matrix(dtm))
  train_bigrams <- as.data.frame(as.matrix(bigrams_dtm))
  
  train <- bind_cols(train_words, train_bigrams)
  
  df$y <- df$uname
  df$uname <- NULL
  train$y <- as.factor(df$y)
  
  train <- train %>% 
    mutate(row_sums = rowSums( .[1: (ncol(train)-1)] ) ) %>% 
    filter(row_sums > 0)
  
  list(processed_df = train, dictionary = c(Terms(dtm), Terms(bigrams_dtm)))
}