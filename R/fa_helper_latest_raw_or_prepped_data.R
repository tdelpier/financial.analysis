

#' FA latest raw or prepped data
#'
#' Selects the latest raw or prepped fa data file. Or can select a speific file. 
#' 
#' 
#'
#' @export
fa_helper_latest_raw_or_prepped_data <- function(raw_prepped_or_file) {
  
  
  if(stringr::str_detect("raw|prepped", raw_prepped_or_file) == TRUE){
      
    data_file <-
      list.files(TannersTools::tt_dir_projects("financial.analysis", "data")) %>% 
      tibble() %>% 
      rename(files = ".") %>% 
      mutate(date.prefix = as.numeric(str_sub(files, 1, 6)),
             raw_or_prepped = ifelse(str_detect(files, "raw"), "raw", ifelse(str_detect(files, "prepped"), "prepped", "neither"))) %>% 
      filter(date.prefix == max(date.prefix)) %>% 
      filter(raw_or_prepped == {{ raw_prepped_or_file }}) %>%
      select(files) %>% 
      pull() 
    
    
  } else {
    data_file <- raw_prepped_or_file
  }
  
  return(data_file)

}  






