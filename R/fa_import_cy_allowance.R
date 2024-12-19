
# state_cat_names <- read_csv("FA/Data_Small/State_Cat_Names.csv")


# CY ALLOWANCE #################################################################


#' @import tidyverse 
#' 
fa_import_cy_allowance <- function() {
  
  
  clean_cy_allow <- function(df, month){
    
    df %>% 
      dplyr::select(FY, month, dcode, icd, descript, amount) %>% 
      dplyr::mutate(amount = amount) %>%
      tidyr::pivot_wider(id_cols = c(FY, dcode, month), 
                  names_from = icd, 
                  values_from = amount,
                  names_prefix = paste0("cy.a.", {{ month }}, ".")) %>% 
      dplyr::mutate(dnum = as.numeric(dcode)) %>% 
      dplyr::select(-dcode)
    
    
  }
  
  cyallow_8_raw <- 
    TannersTools::tt_import_cy_allow(month = 8) %>% 
    dplyr::mutate(icd = ifelse(stringr::str_length(icd) == 2, paste0("0", icd), icd),
           icd = ifelse(stringr::str_length(icd) == 1, paste0("00", icd), icd)) 
  
  cyallow_8 <- 
    cyallow_8_raw %>% 
    clean_cy_allow(month = 8) %>% 
    select(-month)
  
  
  cyallow_1_raw <- 
    TannersTools::tt_import_cy_allow(month = 1) %>% 
    mutate(icd = ifelse(stringr::str_length(icd) == 2, paste0("0", icd), icd),
           icd = ifelse(stringr::str_length(icd) == 1, paste0("00", icd), icd)) 
  
  cyallow_1 <- 
    cyallow_1_raw %>% 
    clean_cy_allow(month = 1) %>% 
    dplyr::select(-month)
  
  cyallow <- 
    cyallow_8 %>% 
    dplyr::full_join(cyallow_1, by = join_by(FY, dnum)) %>% 
    dplyr::select(FY, dnum, everything())
  
  return(cyallow)
  
}

## function to bring in names
# cyallow_names <- 
#   cyallow_1_raw %>% 
#   bind_rows(cyallow_8_raw) %>% 
#   mutate(descript = str_trim(tolower(descript)), side = "both") %>% 
#   select(FY, icd, descript) %>% 
#   distinct(icd, descript, .keep_all = TRUE) %>% 
#   arrange(icd) %>% 
#   group_by(icd) %>% 
#   filter(FY == max(FY)) %>% 
#   select(-FY) %>% 
#   ungroup() %>% 
#   mutate(descript = str_to_title(descript),
#          descript = str_replace_all(descript, "Mpsers", "MPSERS"),
#          descript = str_replace_all(descript, "Uaal", "UAAL"),
#          descript = str_replace_all(descript, "Cte ", "CTE "))


