



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
    # filter(dnum == 52170) %>% 
    select(FY, dnum, starts_with("fid.trans"), 
           starts_with("fid.b"), 
           -ends_with(c("pct", "exp", "chg", "rev"))) %>% 
    pivot_longer(cols = c(starts_with("fid.trans"), starts_with("fid.b"))) %>% 
    mutate(fund = as.numeric(str_sub(name, -2, -1)),
           type = ifelse(str_detect(name, "fid.trans.transfer.to.fund"),"trans.to",
                         ifelse(str_detect(name, "fid.b.fb"), "fb", "none"))) %>% 
    group_by(FY, dnum, fund, type) %>% 
    summarise(value = sum(value, na.rm= TRUE)) %>% 
    filter(value != 0,
           !is.na(value)) %>% 
    pivot_wider(id_cols = c(FY, dnum, fund), names_from = type, values_from = value) %>% 
    mutate(trans.to = ifelse(is.na(trans.to), 0, trans.to),
           fb = ifelse(is.na(fb), 0, fb)) %>% 
    left_join(TannersTools::FID_names_fund, by = "fund")

  
}






