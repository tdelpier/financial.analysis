




# JOINING ######################################################################


fa_join <- function() {
  

  # CY
  cydata <- fa_import_cy_data()
  cyallow <- fa_import_cy_allowance()
  
  
  # Budget
  district_budgets <- 
    TannersTools::tt_import_budget() %>% 
    mutate(dnum = as.numeric(dcode)) %>% 
    select(-dcode, -db.lindsay.notes, -db.tanner.notes)
  
  
  # ID
  district_id <- 
    TannersTools::district_id %>% 
    mutate(dnum = as.numeric(dcode)) %>% 
    select(dnum, everything())
  
  
  
  
  # FID
  rev <- tt_import_fid_rev()
  bal <- fa_import_fid_fund_balance()
  exp <- fa_import_fid_exp()
  trn <- fa_import_fid_transfers()
  
  
  fid <- 
    rev %>% 
    full_join(bal, by = join_by(FY, dnum)) %>% 
    full_join(exp, by = join_by(FY, dnum)) %>% 
    full_join(trn, by = join_by(FY, dnum)) %>% 
    full_join(cydata, by = join_by(FY, dnum)) %>% 
    full_join(cyallow, by = join_by(FY, dnum)) %>% 
    full_join(district_budgets, by = join_by(FY, dnum)) %>% 
    full_join(district_id, by = join_by(dnum))
  
  return(fid)
  
  
}


xxx <- fa_join()




path <- "FA/Data_Small/ESSER_Allocation.xlsx"

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
