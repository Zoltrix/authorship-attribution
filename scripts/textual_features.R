#Problematic features: User aggregated features like the avg number of words
#are not easily computed for the test set

add_textual_features <- function(users) {
  
  special_characters <- "~, $, %, ^, &, *, -, _, = ,+, >, <, [, ], {, }, \\/, \\, |"
  
  user_tokens <- users %>% 
    unnest_tokens(word, text, token = stringr::str_split, pattern = " ", drop = FALSE) %>% 
    group_by(uname, file_id, text) %>%
    mutate(
      new_lines = str_count(word, "\\\\n"),
      special_characters = str_count(word, "[\\~\\$\\%\\^\\&\\*\\-_=\\+\\>\\<\\[\\]\\{\\}\\/\\\\\\|]") - new_lines,
      n_char = nchar(word),
      n_spaces = str_count(text, " "),
      n_numbers = str_count(word, "[\u0660-\u06690-9]"), 
      n_distinct_words = length(unique(word))
      #TODO- emojis
      
    ) %>% ungroup()
  
  # user.agg <- user_tokens %>% 
  #   group_by(uname) %>% 
  #   summarise(
  #     avg_word_length = mean(n_chars),
  #     max_word_length = max(n_chars)
  #   )
  
  user_tokens.agg <- user_tokens %>% 
    group_by(uname, file_id) %>% 
    summarise(
      newlines = sum(new_lines),
      special_chars = sum(special_characters),
      n_spaces = last(n_spaces),
      n_distinct_words = last(n_distinct_words),
      n_chars = sum(n_char) + n_spaces,
      n_numbers = sum(n_numbers),
      max_word_length = max(n_char),
      n_words = n()
    )
  
  users %>% 
    # inner_join(user.agg) %>% 
    inner_join(user_tokens.agg)
}