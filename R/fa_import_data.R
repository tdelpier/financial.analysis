


#' FA Imports FA Data
#'
#' Imports the latest "raw" or "prepped" data. Alternatively, can specify an individual file.
#'
#' @export
fa_import_data <- function(file) {
  
  fa_data_file <- fa_helper_latest_raw_or_prepped_data({{ file }})
  
  path <- TannersTools::tt_dir_projects("financial.analysis", "data", fa_data_file) 
  
  fa_data <- readRDS(path)
  
  
  message("FA data read from: ", path)
  message("Data Import Complete ✓")
  return(fa_data)

  
}





