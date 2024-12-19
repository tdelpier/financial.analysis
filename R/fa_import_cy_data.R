

# CY DATA ######################################################################


fa_import_cy_data <- function() {
  
  
  clean_cy_data <- function(df, month) {
    df %>%
      dplyr::select(-starts_with("x")) %>%
      dplyr::mutate(dnum = as.numeric(dcode)) %>% 
      dplyr::rename_with(.cols = everything(),
                  function(x){paste0("cy.d.", {{ month }}, ".", x)}) %>%
      dplyr::rename(dcode = pastecy.d.8.dcode,
             FY = cy.d.8.FY,
             dnum = cy.d.8.dnum)
    
  }
  
  cydata_8 <- 
    TannersTools::tt_import_cy_data(8) %>% 
    dplyr::select(-starts_with("x")) %>%
    dplyr::mutate(dnum = as.numeric(dcode)) %>%
    dplyr::rename_with(.cols = everything(),
                function(x){paste0("cy.d.8.", x)}) %>%
    dplyr::rename(dcode = cy.d.8.dcode,
           FY = cy.d.8.FY,
           dnum = cy.d.8.dnum)
  
  cydata_1 <- 
    TannersTools::tt_import_cy_data(1)  %>% 
    dplyr::select(-starts_with("x")) %>%
    dplyr::mutate(dnum = as.numeric(dcode)) %>%
    dplyr::rename_with(.cols = everything(),
                function(x){paste0("cy.d.1.", x)}) %>%
    dplyr::rename(dcode = cy.d.1.dcode,
           FY = cy.d.1.FY,
           dnum = cy.d.1.dnum)
  
  
  cydata <- 
    cydata_8 %>% 
    dplyr::full_join(cydata_1, by = dplyr::join_by(dnum, FY)) %>% 
    dplyr::filter(!is.na(dnum))
  
  
  return(cydata)
  
}


