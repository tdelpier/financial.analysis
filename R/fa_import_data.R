




#' FA Imports FA Data
#'
#' By default, this function imports the latest data constructed by date. 
#' Alternatively you may enter the name of a file in the data folder.  
#'
#' @export
fa_import_data <- function(file = "latest") {
  
  
  if({{ file }} == "latest") {
    
    fa_data_file <- 
      list.files(TannersTools::tt_dir_projects("financial.analysis", "data")) %>% 
      tibble() %>% 
      rename(files = ".") %>% 
      mutate(date.prefix = as.numeric(str_sub(files, 1, 6))) %>% 
      filter(date.prefix == max(date.prefix)) %>% 
      select(files) %>% 
      pull()
    
  } else {
    
    fa_data_file <- {{ file }}
    
  }

  
  path <- TannersTools::tt_dir_projects("financial.analysis", "data", fa_data_file) 
  
  
  fa_data <- readRDS(path)
  message("Data Import Complete ✓")
  message("Read from: ", path)
  return(fa_data)

  
  
}

