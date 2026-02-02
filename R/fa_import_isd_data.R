
# 
# isd_cydata <- 
#   TannersTools::tt_import_cy_data(8) %>% 
#   mutate(dnum = as.numeric(dcode)) %>% 
#   TannersTools::tt_dnum_isd(dnum) %>% 
#   filter(flag.isd == 1)
# 
# 
# isd_dnums_dnames <-
#   isd_cydata %>% 
#   filter(FY == fiscal.year - 1) %>% 
#   select(dnum, dname) 
# 
# 
# isd_dnums <- 
#   isd_cydata %>% 
#   filter(FY == fiscal.year - 1) %>% 
#   select(dnum) %>% 
#   pull()



#' FA import data for ISD financial analysis
#'
#'
#' @export
fa_import_isd_data <- function() {
  
  
  tt_dir_rd()
  
  
  isd_cydata <- 
    TannersTools::tt_import_cy_data(8) %>% 
    mutate(dnum = as.numeric(dcode)) %>% 
    tt_dnum_isd(dnum) %>% 
    filter(flag.isd == 1)
  
  isd_taxes <- 
    isd_cydata %>% 
    mutate(fund = 11) %>%  # taxable value isn't in a fund, but I don't want to duplicate
    select(FY, dnum, fund, milloper, enhancemnt, millopdebt, millspeced, millvoced, sev)
  
  isd_dnums_dnames <- 
    isd_cydata %>% 
    filter(FY == fiscal.year - 1) %>% 
    select(dnum, dname)
  
  
  isd_dnums <- 
    isd_dnums_dnames %>% 
    select(dnum) %>% 
    pull()
  
  
  
  
  fidr <- tt_import_fid_R() %>% filter(dnum %in% isd_dnums)
  fide <- tt_import_fid_E() %>% filter(dnum %in% isd_dnums)
  fidb <- tt_import_fid_B() %>% filter(dnum %in% isd_dnums)
  
  
  
  rev <- 
    fidr %>% 
    mutate(flag.transfer = ifelse(majorclass >= 600, "transfer.in", "rev")) %>% 
    group_by(FY, fund, dnum, flag.transfer) %>% 
    summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    pivot_wider(id_cols = c(FY, fund, dnum), 
                values_from = amount, 
                names_from = flag.transfer,
                values_fill = 0)
  
  exp <- 
    fide %>% 
    mutate(flag.transfer = ifelse(object >= 8000, "transfer.out", "exp")) %>% 
    group_by(FY, fund, dnum, flag.transfer) %>% 
    summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    pivot_wider(id_cols = c(FY, fund, dnum), 
                values_from = amount, 
                names_from = flag.transfer,
                values_fill = 0)
  
  bal <- 
    fidb %>% 
    mutate(amount = amount * -1) %>% 
    filter(majorclass >= 700,
           majorclass < 760) %>% 
    group_by(FY, fund, dnum) %>% 
    summarise(bal = sum(amount))
  

  isd_data <<- 
    rev %>% 
    full_join(exp, join_by(FY, dnum, fund)) %>% 
    full_join(bal, join_by(FY, dnum, fund)) %>% 
    full_join(isd_taxes, join_by(FY, dnum, fund)) %>%
    mutate(bal.pct.rev = bal / rev * 100,
           bal.pct.exp = bal/ exp * 100) %>% 
    replace(is.na(.), 0) %>% 
    full_join(isd_dnums_dnames, join_by(dnum)) 
    
  
  # transfers 
  fund_names <- TannersTools::FID_names_fund %>% select(fund, fund.name)
  
  
  isd_transfers <<- 
    fide %>% 
    filter(FY == 2024,
           object > 8000,
           object < 9000) %>% 
    mutate(tranfer.to.fund.number = ifelse(object.2 == 8100, str_sub(func, -2, -1), NA),
           tranfer.to.fund.number = as.numeric(tranfer.to.fund.number),
           tranfer.to.fund.number = ifelse(object.2 >= 8200, object.2, tranfer.to.fund.number),
           tranfer.to.fund.number = as.numeric(tranfer.to.fund.number)) %>% 
    group_by(dnum, fund, tranfer.to.fund.number) %>%
    summarise(amount = sum(amount)) %>% 
    left_join(fund_names, join_by(fund)) %>% 
    rename(transfer.from.fund.name = fund.name,
           transfer.from.fund = fund,
           fund = tranfer.to.fund.number) %>% 
    left_join(fund_names, join_by(fund)) %>% 
    rename(transfer.to.fund = fund,
           transfer.to.fund.name = fund.name) %>% 
    mutate(transfer.to.fund.name = ifelse(transfer.to.fund == 8200, "Payment to other districts", transfer.to.fund.name),
           transfer.to.fund.name = ifelse(transfer.to.fund == 8900, "Other Transactions", transfer.to.fund.name),
           transfer.to.fund.name = ifelse(transfer.to.fund == 8500, "Sub-grantee flow through", transfer.to.fund.name))
  
  
  
}





