
# state_cat_names <- read_csv("FA/Data_Small/State_Cat_Names.csv")


# CY ALLOWANCE #################################################################




#' FA import cy allowance
#'
#'
#' @export
fa_import_cy_allowance <- function() {
  
  
  clean_cy_allow <- function(df, month){
    
    df %>% 
      

      dplyr::mutate(icd.num = as.numeric(icd),
                    
                    # grouping all the deductions together into icd 999
                    icd = ifelse(icd.num %in% 
                                   c(976, 980, 981, 983, 989, 903, 950, 
                                     951, 952, 953, 975, 982, 985, 990, 
                                     991, 992, 993, 994, 995, 996, 997),
                                 "9999", icd),
                    
                    # grouping all the one-time or non-GF recurring revenue
                    icd = ifelse(icd.num %in% 
                                   c(172, 228, 251, 175, 725, 727, 728, 734, 739,
                                     740, 774, 853, 860, 866, 867, 868, 869, 870, 872,
                                     876, 879), 
                                 "8888", icd)) %>%
  
      
      
      
      
      
      group_by(FY, month, dcode, icd) %>% 
      summarise(amount = sum(amount, na.rm = TRUE)) %>% 
      ungroup() %>%
      dplyr::select(FY, month, dcode, icd, amount) %>%
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


