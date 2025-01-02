



#' FA Data Work
#'
#' 
#' 
#'
#' @export
fa_dwork <- function(df) {
  
  fiscal.year <- 2024
  # I need to change this at some point
    # it can't be automatic but I don't really want to mess with it very often
    # needs to be obvious where to change it. 
  
  df %>% 
    fa_dwork_clean() %>%
    fa_dwork_est() %>% 
    fa_dwork_year_changes() %>% 
    fa_dwork_end() %>%
    fa_helper_write_fa_to_data_dir("fa_data_prepped.rds")
  

}



