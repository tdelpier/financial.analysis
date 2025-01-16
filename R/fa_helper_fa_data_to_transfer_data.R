



#' Convert FA_Data to transfer data
#'
#' 
#' ## Uses
#' 
#' this is used to produce the transfer page
#' 
#'
#' @export
fa_helper_fa_data_to_transfer_data <- function(df) {
  
  df %>% 
    ungroup() %>% 
    select(FY, dnum, starts_with("fid.trans"), 
           starts_with("fid.b"), 
           -ends_with(c("pct", "exp", "chg", "rev"))) %>% 
    pivot_longer(cols = c(starts_with("fid.trans"), starts_with("fid.b"))) %>% 
    mutate(fund = str_remove(name, "fid.trans.transfer.to.fund."),
           fund = str_remove(fund, "fid.b.fb"),
           fund = str_remove(fund, ".restricted"),
           fund = as.numeric(fund),
           type = ifelse(str_detect(name, "fid.trans.transfer.to.fund."), "trans.to", NA),
           type = ifelse(str_detect(name, "fid.b.fb"), "fb", type),
           type = ifelse(str_detect(name, "restricted"), "restricted", type)) %>% 
    group_by(FY, dnum, fund, type) %>% 
    summarise(value = sum(value, na.rm= TRUE)) %>% 
    pivot_wider(id_cols = c(FY, dnum, fund), names_from = type, values_from = value) %>% 
    left_join(TannersTools::FID_names_fund, by = "fund") %>% 
    mutate(trans.to = ifelse(is.na(trans.to), 0, trans.to),
           fb = ifelse(is.na(fb), 0, fb)) %>%
    rename(fund.group.trans.to = fund.group,
           fund.name.trans.to = fund.name,
           fund.balance = fb,
           transfered.from.gf = trans.to,
           fund.balance.restricted = restricted) %>% 
    mutate(fund.balance.unrestricted = fund.balance - fund.balance.restricted) %>% 
    filter(fund.balance != 0 | 
             fund.balance.restricted != 0 | 
             fund.balance.unrestricted != 0 |
             transfered.from.gf != 0) 
  
  
}
