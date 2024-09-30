


# TRANSFER #####################################################################

fa_helper_add_fund_names <- function(df, fund.digit) {
  
  df %>% 
    left_join(TannersTools::FID_names_fund, by = join_by({{ fund.digit }} == fund))

  
}




fa_import_fid_transfers <- function(fund = 11) {
  
  
  fid.e.trans.out <- 
    tt_import_fid_E() %>% 
    filter(fund == {{ fund }}) %>% 
    filter(func.1 == 600) %>% 
    group_by(FY, dnum, fund, func) %>% 
    summarise(amount.exp = sum(amount)) %>% 
    ungroup() %>% 
    mutate(fund.trans.to = as.numeric(str_sub(func, 2, 3)),
           amount.exp = as.integer(amount.exp)) %>% 
    filter(fund == {{ fund }},
           amount.exp > 1) %>% 
    fa_helper_add_fund_names(fund.trans.to) %>% 
    rename(fund.group.trans.to = fund.group,
           fund.name.trans.to = fund.name) %>% 
    group_by(FY, dnum, fund.group.trans.to, fund.name.trans.to) %>% 
    summarise(amount.exp = sum(amount.exp, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(dnum, FY) %>% 
    select(-fund.name.trans.to) %>% 
    pivot_wider(id_cols = c(FY, dnum), names_from = fund.group.trans.to, values_from = amount.exp, names_prefix = "fid.trans.transfer.to.fund.")
  
  
  
  return(fid.e.trans.out)
  
  
}





fa_helper_fid_transfer_long  <- function(df) {
  
  df %>% 
    select(FY, dnum, dplyr::starts_with("fid.trans.transfer.to.fund.")) %>% 
    pivot_longer(cols = c(dplyr::starts_with("fid.trans.transfer.to.fund.")), 
                 values_drop_na = TRUE,
                 names_prefix = "fid.trans.transfer.to.fund.") %>% 
    rename(amount.exp = value,
           fund.group.trans.to = name) %>% 
    mutate(fund.group.trans.to = as.numeric(fund.group.trans.to)) %>% 
    fa_helper_add_fund_names(fund.group.trans.to) %>% 
    rename(fund.name.trans.to = fund.name) %>% 
    select(FY, dnum, fund.group.trans.to, fund.name.trans.to, amount.exp)
  
}
 

# x1 <- fa_import_fid_transfers()
# x2 <- x1 %>% fa_helper_fid_transfer_long()

 
  

