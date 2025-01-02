



#' FA Join all data
#'
#' @export
fa_helper_write_fa_to_data_dir <- function(df, suffix) {
  
  path <- TannersTools::tt_dir_projects("financial.analysis", 
                                        "data",TannersTools::tt_date_prefix(suffix))
  
  readr::write_rds(x = df,  file = path)
  
  message(paste0("Data contructed: ", path))
  
  
}