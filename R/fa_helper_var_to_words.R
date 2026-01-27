




#' FA Variable to Words
#'
#' converts FA_Data_District variables to words for use in dynamic text
#'
#' @export
var_to_words <- function(var) {
  
  FA_Data_District %>% 
    filter(FY == max(FY) - 1) %>% 
    select({{ var }}) %>% 
    unique() %>% 
    pull()
  
}