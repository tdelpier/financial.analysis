



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
    mutate(fund = ifelse(str_detect(name, "fid.trans.transfer.to.fund."), str_remove(name, "fid.trans.transfer.to.fund."), NA),
           fund = ifelse(is.na(fund), str_sub(name, 10, 11) %>% as.numeric(), fund),
           fund = as.numeric(fund),
           type = ifelse(str_detect(name, "fid.trans.transfer.to.fund."), "trans.to", NA),
           type = ifelse(str_detect(name, "fid.b.fb."), "fb.total", type),
           type = ifelse(str_detect(name, "nonspendable"), "fb.nonspendable", type),
           
           type = ifelse(str_detect(name, "restricted"), "fb.restricted", type),
           type = ifelse(str_detect(name, "committed"), "fb.committed", type),
           type = ifelse(str_detect(name, "assigned"), "fb.assigned", type),
           type = ifelse(str_detect(name, "unassigned"), "fb.unassigned", type)
    ) %>% 
    group_by(FY, dnum, fund, type) %>%
    summarise(value = sum(value, na.rm= TRUE)) %>% 
    pivot_wider(id_cols = c(FY, dnum, fund), names_from = type, values_from = value) %>%
    left_join(TannersTools::FID_names_fund, by = "fund") %>% 
    rename(fund.group.trans.to = fund.group,
           fund.name.trans.to = fund.name,
           transfered.from.gf = trans.to) %>%
    mutate(fund.balance.unrestricted = fb.unassigned + fb.assigned + fb.committed) 
  
  
}





