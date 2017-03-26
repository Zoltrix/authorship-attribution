read_data <- function(path) {
  full_path <- list.files("data/sample/users/", recursive = T, full.names = T)
  relative_path <- list.files("data/sample/users/", recursive = T)
  
  users_files <- data.frame(full_path, relative_path)
  
  users_files %>% 
    separate(relative_path, into = c("uname", "file_id"), sep = "[\\/\\.]") %>% 
    rowwise() %>% 
    mutate(text = read_file(as.character(full_path))) %>% 
    ungroup() %>% 
    select(-full_path)
}