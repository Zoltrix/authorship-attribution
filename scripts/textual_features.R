add_textual_features <- function(users) {
  
  special_characters <- "~, $, %, ^, &, *, -, _, = ,+, >, <, [, ], {, }, \\/, \\, |"
  
  user_tokens <- users %>% 
    unnest_tokens(word, text, token = stringr::str_split, pattern = " ", drop = FALSE) %>% 
    group_by(uname, file_id, text) %>%
    mutate(
      new_lines = str_count(word, "\\\\n"),
      special_characters = str_count(word, "[\\~\\$\\%\\^\\&\\*\\-_=\\+\\>\\<\\[\\]\\{\\}\\/\\\\\\|]") - new_lines,
      n_chars = nchar(word),
      n_spaces = str_count(text, " "),
      n_numbers = str_count(word, "[\u0660-\u06690-9]")
      #TODO- emojis
      
    ) %>% ungroup()
  
  user_tokens.agg <- user_tokens %>% 
    group_by(uname, file_id) %>% 
    summarise(
      newlines = sum(new_lines),
      special_chars = sum(special_characters),
      n_spaces = last(n_spaces), 
      n_chars = sum(n_chars) + n_spaces,
      n_numbers = sum(n_numbers),
      n_words = n()
    )
  
  users %>% 
    inner_join(user_tokens.agg)
}