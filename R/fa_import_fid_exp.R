

# EXPENDITURE ##################################################################

fa_import_fid_exp <- function(fund = 11) {
  
  
  fid_exp_raw <- 
    tt_import_fid_E() %>% 
    filter(fund == {{ fund }})
  
  
  filter_out_stephenson_problem <- function(df){
    
    df %>% 
    mutate(stephenson.problem = ifelse(dnum == 55120 & object.2 == 8500, 1, 0)) %>% 
      filter(stephenson.problem == 0) %>% 
      select(-stephenson.problem) 
    
    
  }
  
  
  
  FID_Exp_Total <- 
    fid_exp_raw %>% 
    filter(fund == {{ fund }}) %>% 
    mutate(transfer_flag = ifelse(func.1 == 600, 1, 0),
           transfer = ifelse(transfer_flag == 1, amount, 0),
           amount = ifelse(transfer_flag == 0, amount, 0)) %>% 
    filter_out_stephenson_problem() %>% 
    group_by(FY, dnum) %>%
    summarise(fid.e.total.xtrans = sum(amount),
              fid.e.trans = sum(transfer)) %>% 
    mutate(fid.e.total = fid.e.total.xtrans + fid.e.trans) %>% 
    ungroup()
  
  
  
  FID_Exp_Stim <- 
    fid_exp_raw %>% 
    filter(fund == {{ fund }}) %>%
    mutate(stim.name = case_when(grant == 796 ~ "esser1",
                                 grant == 485 ~ "esser2",
                                 grant == 435 ~ "esser3",
                                 grant == 499 ~ "geer1",
                                 grant == 800 ~ "equity",
                                 grant == 798 ~ "crf1",
                                 grant == 799 ~ "crf2",
                                 grant == 387 ~ "essereq2",
                                 grant == 441 ~ "essereq3")) %>% 
    group_by(FY, dnum, stim.name) %>%
    summarise(amount.exp = sum(amount)) %>%
    ungroup() %>% 
    add_row(FY = 2004, dnum = 1010, stim.name = "essereq3", amount.exp = 1) %>%
    group_by(FY, dnum, stim.name) %>%
    filter(!is.na(stim.name)) %>% 
    pivot_wider(id_cols = c(FY, dnum), 
                values_from = amount.exp, 
                names_from = stim.name, 
                names_prefix = "fid.e.fed.stim.") %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  
  FID_Exp <- 
    FID_Exp_Total %>% 
    full_join(FID_Exp_Stim, by = join_by(dnum, FY))
  
  return(FID_Exp)
  
  
}


# t1 <- Sys.time()
# x1 <- fa_import_fid_exp()
# t2 <- Sys.time()
# t2-t1


