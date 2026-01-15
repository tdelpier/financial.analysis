



# state_cat_names <- read_csv("FA/Data_Small/State_Cat_Names.csv")


# CY ALLOWANCE #################################################################



#' FA import cy adjustment data
#'
#' unlike my cy allow and cy data imports, this one is just for FY 2026. I don't have a full system dealing with adjustment data. 
#'
#' @export
fa_import_cy_adjust <- function() {
  
  if(fiscal.year %in% c(2025, 2026, 2027)) {
    
    cyadjust <- 
      read_excel(tt_dir_data("CY Data/2026-01/January2026/PublicFilesCYAdjustment__Report.xlsx")) %>% 
      janitor::clean_names(sep_out = ".") %>% 
      filter(icd %in% c(79, 567, 858)) %>% 
      mutate(dnum = as.numeric(dcode),
             FY = 2026) %>% 
      select(FY, dnum, icd, descript, amount) %>% 
      mutate(icd = ifelse(stringr::str_length(icd) == 2, paste0("0", icd), icd),
             icd = ifelse(stringr::str_length(icd) == 1, paste0("00", icd), icd)) %>% 
      group_by(FY, dnum, icd) %>% 
      summarise(amount = sum(amount, na.rm = TRUE)) %>% 
      ungroup() %>%
      dplyr::select(FY, dnum, icd, amount) %>%
      tidyr::pivot_wider(id_cols = c(FY, dnum),
                         names_from = icd,
                         values_from = amount,
                         names_prefix = paste0("cy.adj.1.")) 
    
    return(cyadjust)
    
  }
  

}


