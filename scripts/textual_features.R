add_textual_features <- function(users) {
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
  
  users %>% 
    inner_join(user_tokens.agg)
}