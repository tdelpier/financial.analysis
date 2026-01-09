


#' FA Imports Geographic Data
#'
#' Imports geographic data
#'
#' @export
fa_import_geo <- function() {
  
  sf::sf_use_s2(FALSE)
  
  tt_import_geo_district() %>% 
    sf::st_set_crs(4269) %>% 
    sf::st_transform(4269) %>%   
    # smoothr::drop_crumbs(threshold = units::set_units(1, km^2)) %>%
      # dropping crumbs was causing an error that I couldn't figure out
    mutate(c_id = row_number()) %>%   
    # st_crop(xmin = -82.4, xmax = -90.35, ymin = 41.7, ymax = 47.5) %>%
    sf::st_make_valid()
  
}


