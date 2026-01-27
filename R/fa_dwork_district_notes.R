

#' add a district note
#' 
#' template to add at district note
#'
#'@export
fa_helper_add_district_notes <- function(df, dnum, note) {
  
  df %>% 
    dplyr::mutate(district.notes = ifelse(FY == fiscal.year & dnum == {{ dnum }},
                                          {{ note }},
                                          district.notes))
  
}



#' Remove duplicated strings
#'
#'@export
fa_dwork_district_notes <- function(df) {
  
  df %>%
   
  # districts with "Funded Project" or "Federal, State, and Local Grant Funds" aggregated into GF by FID
    
    # Dearborn
    fa_helper_add_district_notes(82030, 'The district maintains a "Funded Projects" fund that is not recognized by state reporting in the FID. This funded is aggregated into the General Fund. Budget data for the "Funded Projects" fund was not in FY 2015-2017, 2019, or 2020.') %>% 
    
    # Taylor
    fa_helper_add_district_notes(82150, 'The district maintains a "Federal, State, and Local Grants" fund that is not recognized by state reporting in the FID. This funded is aggregated into the General Fund.' ) %>% 
    
    # Wayne-Westland
    fa_helper_add_district_notes(82160, 'The district maintains a "Federal, State, and Local Grants" fund that is not recognized by state reporting in the FID. This funded is aggregated into the General Fund.' ) %>% 
    
    # Lincoln Park
    fa_helper_add_district_notes(82090, 'The district maintains a "Funded Projects" fund that is not recognized by state reporting in the FID. This funded is aggregated into the General Fund.') %>% 
    
    # Livonia
    fa_helper_add_district_notes(82095, 'The district maintains a "Funded Projects" fund that is not recognized by state reporting in the FID. This funded is aggregated into the General Fund.') %>% 
    
    # Plymouth Canton
    fa_helper_add_district_notes(82100, 'The district maintains a "Funded Projects" fund that is not recognized by state reporting in the FID. This funded is aggregated into the General Fund. Budget data for the "Funded Projects" fund was not in FY 2015-2019.') %>% 
    
    
  # Other Notes
    
    # Baldwin
    fa_helper_add_district_notes(43040, "Baldwin changed their fund structure in 2019 resulting in a large reduction in both General Fund revenue and expenditure.") 
    
   
    
    
}
