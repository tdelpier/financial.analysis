


# JOINING ######################################################################



#' FA Join all data
#'
#'
#' @export
fa_construct_data <- function(path) {
  

  # CY
  cydata <- fa_import_cy_data()
  cyallow <- fa_import_cy_allowance()
  message("cy done")
  
  
  # Budget
  district_budgets <- 
    TannersTools::tt_import_budget() %>% 
    dplyr::mutate(dnum = as.numeric(dcode)) %>% 
    dplyr::select(-dcode, -db.lindsay.notes, -db.tanner.notes)
  message("budget done")
  
  
  # ID
  district_id <- 
    TannersTools::district_id %>% 
    dplyr::mutate(dnum = as.numeric(dcode)) %>% 
    dplyr::select(dnum, tidyselect::everything())
  message("ID done")
  
  

  # FID
  rev <- fa_import_fid_rev()
  message("FID rev")
  bal <- fa_import_fid_fund_balance()
  message("FID bal")
  exp <- fa_import_fid_exp()
  message("FID exp")
  trn <- fa_import_fid_transfers()
  message("FID transfer")
  
  
  fa_data <- 
    rev %>% 
    dplyr::full_join(bal, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(exp, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(trn, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(cydata, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(cyallow, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(district_budgets, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(district_id, by = dplyr::join_by(dnum))
  
  
  path <- TannersTools::tt_dir_projects("financial.analysis", "data",TannersTools::tt_date_prefix("fa_data.rds"))
  
  readr::write_rds(x = fa_data,  file = path)
  
  message(paste0("Data contructed: ", path))
  
}






# 
# path <- "FA/Data_Small/ESSER_Allocation.xlsx"
# 
# ESSER1 <- path %>% read_excel(sheet = "ESSER1")
# ESSER2 <- path %>% read_excel(sheet = "ESSER2")
# ESSER3 <- path %>% read_excel(sheet = "ESSER3")
# GEER1 <- path %>% read_excel(sheet = "GEER1")
# Equity <- path %>% read_excel(sheet = "Equity")
# CRF1 <- path %>% read_excel(sheet = "CRF1")
# CRF2 <- path %>% read_excel(sheet = "CRF2")
# ESSEReq2 <- path %>% read_excel(sheet = "ESSEReq2")
# ESSEReq3 <- path %>% read_excel(sheet = "ESSEReq3")
# 
# Stim <- ESSER1 %>% 
#   full_join(ESSER2, by = "dcode") %>% 
#   full_join(ESSER3, by = "dcode") %>% 
#   full_join(GEER1, by = "dcode") %>% 
#   full_join(Equity, by = "dcode") %>% 
#   full_join(CRF1, by = "dcode") %>% 
#   full_join(CRF2, by = "dcode") %>% 
#   full_join(ESSEReq2, by = "dcode") %>% 
#   full_join(ESSEReq3, by = "dcode") %>% 
#   mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
#   mutate(dnum = as.numeric(dcode)) %>% 
#   select(-dcode)
