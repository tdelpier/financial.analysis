


# TRANSFER #####################################################################

fa_helper_add_fund_names <- function(df, fund.digit) {
  
  df %>% 
    left_join(TannersTools::FID_names_fund, by = join_by({{ fund.digit }} == fund))

  
}



fa_import_fid_transfers <- function(fund = 11) {
  
  
  fid.e.trans.out <- 
    TannersTools::tt_import_fid_E() %>% 
    dplyr::filter(fund == {{ fund }}) %>% 
    dplyr::filter(func.1 == 600) %>% 
    dplyr::group_by(FY, dnum, fund, func) %>% 
    dplyr::summarise(amount.exp = sum(amount)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(fund.trans.to = as.numeric(stringr::str_sub(func, 2, 3)),
           amount.exp = as.integer(amount.exp)) %>% 
    dplyr::filter(fund == {{ fund }},
           amount.exp > 1) %>% 
    fa_helper_add_fund_names(fund.trans.to) %>% 
    dplyr::rename(fund.group.trans.to = fund.group,
           fund.name.trans.to = fund.name) %>% 
    dplyr::group_by(FY, dnum, fund.group.trans.to, fund.name.trans.to) %>% 
    dplyr::summarise(amount.exp = sum(amount.exp, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(dnum, FY) %>% 
    dplyr::select(-fund.name.trans.to) %>% 
    tidyr::pivot_wider(id_cols = c(FY, dnum), names_from = fund.group.trans.to, values_from = amount.exp, names_prefix = "fid.trans.transfer.to.fund.")
  
  
  
  return(fid.e.trans.out)
  
  
}





fa_helper_fid_transfer_long  <- function(df) {
  
  df %>% 
    dplyr::select(FY, dnum, dplyr::starts_with("fid.trans.transfer.to.fund.")) %>% 
    tidyr::pivot_longer(cols = c(dplyr::starts_with("fid.trans.transfer.to.fund.")), 
                 values_drop_na = TRUE,
                 names_prefix = "fid.trans.transfer.to.fund.") %>% 
    dplyr::rename(amount.exp = value,
           fund.group.trans.to = name) %>% 
    dplyr::mutate(fund.group.trans.to = as.numeric(fund.group.trans.to)) %>% 
    fa_helper_add_fund_names(fund.group.trans.to) %>% 
    dplyr::rename(fund.name.trans.to = fund.name) %>% 
    dplyr::select(FY, dnum, fund.group.trans.to, fund.name.trans.to, amount.exp)
  
}
 

# x1 <- fa_import_fid_transfers()
# x2 <- x1 %>% fa_helper_fid_transfer_long()

 
  

