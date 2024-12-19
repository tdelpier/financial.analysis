
  
  # EXPENDITURE ##################################################################

#' FA FID exp
#'
#' @export  
fa_import_fid_exp <- function(fund = 11) {
  
  
  fid_exp_raw <- 
    TannersTools::tt_import_fid_E() %>% 
    dplyr::filter(fund == {{ fund }})
  
  
  filter_out_stephenson_problem <- function(df){
    
    df %>% 
      dplyr::mutate(stephenson.problem = ifelse(dnum == 55120 & object.2 == 8500, 1, 0)) %>% 
      dplyr::filter(stephenson.problem == 0) %>% 
      dplyr::select(-stephenson.problem) 
    
    
  }
  
  
  
  FID_Exp_Total <- 
    fid_exp_raw %>% 
    dplyr::filter(fund == {{ fund }}) %>% 
    dplyr::mutate(transfer_flag = ifelse(func.1 == 600, 1, 0),
           transfer = ifelse(transfer_flag == 1, amount, 0),
           amount = ifelse(transfer_flag == 0, amount, 0)) %>% 
    filter_out_stephenson_problem() %>% 
    dplyr::group_by(FY, dnum) %>%
    dplyr::summarise(fid.e.total.xtrans = sum(amount),
              fid.e.trans = sum(transfer)) %>% 
    dplyr::mutate(fid.e.total = fid.e.total.xtrans + fid.e.trans) %>% 
    dplyr::ungroup()
  
  
  
  FID_Exp_Stim <- 
    fid_exp_raw %>% 
    dplyr::filter(fund == {{ fund }}) %>%
    dplyr::mutate(stim.name = dplyr::case_when(grant == 796 ~ "esser1",
                                 grant == 485 ~ "esser2",
                                 grant == 435 ~ "esser3",
                                 grant == 499 ~ "geer1",
                                 grant == 800 ~ "equity",
                                 grant == 798 ~ "crf1",
                                 grant == 799 ~ "crf2",
                                 grant == 387 ~ "essereq2",
                                 grant == 441 ~ "essereq3")) %>% 
    dplyr::group_by(FY, dnum, stim.name) %>%
    dplyr::summarise(amount.exp = sum(amount)) %>%
    dplyr::ungroup() %>% 
    tibble::add_row(FY = 2004, dnum = 1010, stim.name = "essereq3", amount.exp = 1) %>%
    group_by(FY, dnum, stim.name) %>%
    dplyr::filter(!is.na(stim.name)) %>% 
    tidyr::pivot_wider(id_cols = c(FY, dnum), 
                values_from = amount.exp, 
                names_from = stim.name, 
                names_prefix = "fid.e.fed.stim.") %>% 
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  
  FID_Exp <- 
    FID_Exp_Total %>% 
    dplyr::full_join(FID_Exp_Stim, by = join_by(dnum, FY))
  
  return(FID_Exp)
  
  
}


# t1 <- Sys.time()
# x1 <- fa_import_fid_exp()
# t2 <- Sys.time()
# t2-t1


