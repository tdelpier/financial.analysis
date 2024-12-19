




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
  FA_Data <<- readRDS(path)
  
  
  ## Geo Data
  
  sf_use_s2(FALSE)
  
  dist_geo <<-
    tt_import_geo_district() %>% 
    st_set_crs(4269) %>% 
    st_transform(4269) %>%   
    drop_crumbs(threshold = units::set_units(1, km^2)) %>% 
    mutate(c_id = row_number()) %>%   
    # st_crop(xmin = -82.4, xmax = -90.35, ymin = 41.7, ymax = 47.5) %>%
    st_make_valid()
  
  
  message("FA data read from: ", path)
  message("Data Import Complete ✓")

  
}
