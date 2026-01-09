


# FUND BALANCE #################################################################

#' FA FID Bal
#'
#' @export
fa_import_fid_fund_balance <- function() {
  
  fb_basic <- 
    TannersTools::tt_import_fid_B() %>% 
    dplyr::filter(majorclass >= 700,
                  majorclass < 760,
                  amount != 0,
                  !is.na(amount)) 
  
  fb_type <- 
    fb_basic%>%
    dplyr::mutate(fb.type = case_match(majorclass, 
                                       c(710, 711) ~ "nonspendable",
                                       c(720, 721) ~ "restricted",
                                       c(730, 731) ~ "committed",
                                       c(740, 741) ~ "assigned",
                                       c(750, 751) ~ "unassigned",
                                       .default = "unassigned")) %>% 
    dplyr::group_by(FY, fund, dnum, fb.type) %>% 
    dplyr::summarise(amount = sum(amount, na.rm = TRUE) * -1) %>% 
    dplyr::ungroup() %>% 
    mutate(name = paste0(fund, ".", fb.type)) %>% 
    tidyr::pivot_wider(id_cols = c(FY, dnum), 
                       values_from = amount, 
                       names_from = name,
                       names_prefix = "fid.b.fb.",
                       values_fill = 0)
  
  fb_total <-
    fb_basic %>% 
    dplyr::group_by(FY, fund, dnum) %>% 
    dplyr::summarise(amount = sum(amount, na.rm = TRUE) * -1) %>% 
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(id_cols = c(FY, dnum), 
                       values_from = amount, 
                       names_from = fund,
                       names_prefix = "fid.b.fb.",
                       values_fill = 0)
  
  
  fund_balances <-
    fb_total %>% 
    left_join(fb_type, by = join_by(FY == FY, dnum == dnum))
  
  
  
  return(fund_balances)
  
}


# 
# # fund_balances <- 
#  TannersTools::tt_import_fid_B() %>% 
#   dplyr::filter(majorclass >= 700,
#                 majorclass < 760,
#                 amount != 0,
#                 !is.na(amount)) %>%
#     mutate(restricted = ifelse(majorclass < 730, amount * -1, NA),
#            amount = amount * -1) %>% 
#   dplyr::group_by(FY, fund, dnum) %>% 
#   dplyr::summarise(total = sum(amount, na.rm = TRUE),
#                    .restricted = sum(restricted, na.rm = TRUE)) %>% 
#   dplyr::ungroup() %>% 
#   tidyr::pivot_longer(cols = c(total, .restricted)) %>% 
#    mutate(fund.type = name,
#           fund.type = ifelse(fund.type == "total", "", fund.type),
#           name = paste0(fund, fund.type)) %>% 
#   tidyr::pivot_wider(id_cols = c(FY, dnum),
#                      names_from = name, 
#                      values_from = value, 
#                      names_prefix = "fid.b.fb",
#                      values_fill = 0) 



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


