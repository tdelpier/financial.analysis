
#' Top Level: Data Work
#'
#' This function strings together the dwork functions in this order:
#' [financial.analysis::fa_dwork_clean()], 
#' [financial.analysis::fa_dwork_est()]
#' [financial.analysis::fa_dwork_year_changes()]
#' [financial.analysis::fa_dwork_end()]
#' 
#' Then it exports that "prepped" data with [financial.analysis::fa_helper_write_fa_to_data_dir()]
#' I'm using this as a manual cache so I can start from the prepped data faster
#' 
#' The user can load the "prepped" data with [financial.analysis::fa_import_data()]
#' The argument should be "prepped"
#' 
#' @param df requires raw data to be supplied
#' 
#' # Uses
#' 
#' 'fa_import_data("raw") %>% fa_dwork()'
#' Then the user can load the "prepped" and save it to an object by calling
#' 'FA_Data <- fa_import_data("prepped")'
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
    fa_dwork_flags() %>% 
    fa_dwork_end() %>% 
    fa_dwork_district_notes() %>% 
    
    fa_helper_write_fa_to_data_dir("fa_data_prepped.rds")
  

}

