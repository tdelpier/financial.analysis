


# JOINING ######################################################################



#' FA Join all data
#'
#' @export
fa_construct_data <- function(path) {
  

  # CY
  cydata <- fa_import_cy_data()
  cyallow <- fa_import_cy_allowance()
  cyadjust <- fa_import_cy_adjust()
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
  
  
  # Stimulus Allocations 
  stim <- 
    fa_import_stim()
  message("Stim")
  
  

  # FID
  rev <- fa_import_fid_rev()
  message("FID rev")
  bal <- fa_import_fid_fund_balance()
  message("FID bal")
  exp <- fa_import_fid_exp()
  message("FID exp")
  trn <- fa_import_fid_transfers()
  message("FID transfer")
  
  
  # NRS
  nrs <- fa_import_nrs()
  message("NRS")
  
  
  fa_data <- 
    rev %>% 
    dplyr::full_join(bal, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(exp, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(trn, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(stim, by = dplyr::join_by(dnum), multiple = "all", copy = TRUE) %>%
    dplyr::full_join(cydata, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(cyallow, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(cyadjust, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(district_budgets, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(nrs, by = dplyr::join_by(FY, dnum)) %>% 
    dplyr::full_join(district_id, by = dplyr::join_by(dnum))
  
  
  
  fa_data %>% 
    filter(FY > (fiscal.year - 11)) %>% 
    fa_helper_write_fa_to_data_dir("fa_data_raw.rds")
  
}


