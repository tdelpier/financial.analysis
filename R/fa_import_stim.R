
#' FA Import Stim
#'
#' @export
fa_import_stim <- function() {

  path <- TannersTools::tt_dir_projects("financial.analysis", "stimulus", "ESSER_Allocation.xlsx")
  
  ESSER1 <- path %>% read_excel(sheet = "ESSER1")
  ESSER2 <- path %>% read_excel(sheet = "ESSER2")
  ESSER3 <- path %>% read_excel(sheet = "ESSER3")
  GEER1 <- path %>% read_excel(sheet = "GEER1")
  Equity <- path %>% read_excel(sheet = "Equity")
  CRF1 <- path %>% read_excel(sheet = "CRF1")
  CRF2 <- path %>% read_excel(sheet = "CRF2")
  ESSEReq2 <- path %>% read_excel(sheet = "ESSEReq2")
  ESSEReq3 <- path %>% read_excel(sheet = "ESSEReq3")
  
  Stim <- ESSER1 %>% 
    full_join(ESSER2, by = "dcode") %>% 
    full_join(ESSER3, by = "dcode") %>% 
    full_join(GEER1, by = "dcode") %>% 
    full_join(Equity, by = "dcode") %>% 
    full_join(CRF1, by = "dcode") %>% 
    full_join(CRF2, by = "dcode") %>% 
    full_join(ESSEReq2, by = "dcode") %>% 
    full_join(ESSEReq3, by = "dcode") %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    mutate(dnum = as.numeric(dcode)) %>% 
    select(-dcode)
  
  
}


