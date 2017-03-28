split_data <- function(data, ratio = 0.7, seed = 1234) {
  data <- data %>%
    mutate(id = row_number())
  
  set.seed(seed)
  training <- data %>% 
    group_by(uname) %>% 
    sample_frac(ratio)
  
  testing <- data[!data$id %in% training$id, ]
  
  training$id <- NULL
  testing$id <- NULL
  list(training=training, testing=testing)
}