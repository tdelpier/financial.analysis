

# CY DATA ######################################################################


fa_import_cy_data <- function() {
  
  
  clean_cy_data <- function(df, month) {
    df %>%
      select(-starts_with("x")) %>%
      mutate(dnum = as.numeric(dcode)) %>% 
      rename_with(.cols = everything(),
                  function(x){paste0("cy.d.", {{ month }}, ".", x)}) %>%
      rename(dcode = pastecy.d.8.dcode,
             FY = cy.d.8.FY,
             dnum = cy.d.8.dnum)
    
  }
  
  cydata_8 <- 
    tt_import_cy_data(8) %>% 
    select(-starts_with("x")) %>%
    mutate(dnum = as.numeric(dcode)) %>%
    rename_with(.cols = everything(),
                function(x){paste0("cy.d.8.", x)}) %>%
    rename(dcode = cy.d.8.dcode,
           FY = cy.d.8.FY,
           dnum = cy.d.8.dnum)
  
  cydata_1 <- tt_import_cy_data(1)  %>% 
    select(-starts_with("x")) %>%
    mutate(dnum = as.numeric(dcode)) %>%
    rename_with(.cols = everything(),
                function(x){paste0("cy.d.1.", x)}) %>%
    rename(dcode = cy.d.1.dcode,
           FY = cy.d.1.FY,
           dnum = cy.d.1.dnum)
  
  
  cydata <- 
    cydata_8 %>% 
    full_join(cydata_1, by = join_by(dnum, FY)) %>% 
    filter(!is.na(dnum))
  
  
  return(cydata)
  
}


