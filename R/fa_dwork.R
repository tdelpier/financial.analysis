



#' FA Data Work
#'
#' 
#' 
#'
#' @export
fa_dwork <- function(df) {
  
  fiscal.year <- 2024
  
  FA_Data %>% 
  fa_dwork_clean() %>% 
  fa_dwork_est() %>% 
  fa_dwork_year_changes() %>% 
  fa_dwork_end()


}



