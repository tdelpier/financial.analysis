

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
    fa_helper_add_district_notes(82090, "The district maintains a seperate Special Education fund. ") %>% 
  
    fa_helper_add_district_notes(43040, "Baldwin changed their fund structure in 2019 resulting in a large reduction in both General Fund revenue and expenditure.") 
    
   
    
    
}
