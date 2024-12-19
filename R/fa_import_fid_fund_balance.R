


# FUND BALANCE #################################################################


fa_import_fid_fund_balance <- function() {
  
  fund_balances <- 
    TannersTools::tt_import_fid_B() %>% 
    dplyr::filter(majorclass >= 700) %>% 
    dplyr::mutate(mc_1 = as.numeric(stringr::str_sub(majorclass, 1, 1)),
                  mc_2 = as.numeric(stringr::str_sub(majorclass, 1, 2)),
                  amount.bal = amount * -1) %>% 
    dplyr::group_by(FY, fund, dnum) %>% 
    dplyr::summarise(amount = sum(amount.bal, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(FundBal = amount) %>% 
    tidyr::pivot_wider(id_cols = c(FY, dnum),
                       names_from = fund, 
                       values_from = FundBal, 
                       names_prefix = "fid.b.fb",
                       values_fill = 0)
  
  return(fund_balances)
  
}






# FID_Bal_Detail <- 
# tt_import_fid_B() %>% 
#   filter(majorclass >= 700) %>% 
#   mutate(mc_1 = as.numeric(str_sub(majorclass, 1, 1)),
#          mc_2 = as.numeric(str_sub(majorclass, 1, 2)),
#          amount.bal = amount * -1) %>% 
#   filter(FY == 2024 - 1) %>%
#   group_by(dnum, fund, mc_2) %>% 
#   summarise(amount.bal = sum(amount.bal, na.rm = TRUE)) %>% 
#   arrange(mc_2) %>% 
#   pivot_wider(id_cols = c(dnum, fund), names_from = mc_2, values_from = amount.bal, names_prefix = "mc_") %>% 
#   # mutate(across(everything(), .fns = ~replace_na(.,0))) %>% 
#   mutate(mc_71 = ifelse(is.na(mc_71), 0, mc_71),
#          mc_72 = ifelse(is.na(mc_72), 0, mc_72),
#          mc_73 = ifelse(is.na(mc_73), 0, mc_73),
#          mc_74 = ifelse(is.na(mc_74), 0, mc_74),
#          mc_75 = ifelse(is.na(mc_75), 0, mc_75),
#          mc_76 = ifelse(is.na(mc_76), 0, mc_76),
#          mc_77 = ifelse(is.na(mc_77), 0, mc_77),
#          mc_78 = ifelse(is.na(mc_78), 0, mc_78),
#          mc_79 = ifelse(is.na(mc_79), 0, mc_79),
#          fb.unrestricted = mc_73 + mc_74 + mc_75) %>% 
#   arrange(dnum)

# MC 71x Non Spendable Fund Balance
# MC 72x Restricted Fund Balance
# MC 73x Committed Fund Balance
# MC 74x Assigned Fund Balance
# MC 75x Unassigned Fund Balance


