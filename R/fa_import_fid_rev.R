


#' Import FID Rev
#'
#' This is the classification scheme on the audits 
#' Local			  MC 1xx
#' State 			  MC 3xx
#' Federal			MC 4xx (stimulus is suffix 230 and 250 within MC 4xx)
#' Other			  MC 2xx and MC 500-549
#' Transfers	  MC 6xx and MC 550-599
#'
#'
#'
#' @export
fa_import_fid_rev <- function(fund = 11){
  
  TannersTools::tt_import_fid_R() %>%
    dplyr::filter(fund == {{ fund }}) %>%
    dplyr::mutate(dcode = as.numeric(dcode),
           mc_1 = stringr::str_sub(majorclass, 1, 1),
           mc_2 = as.numeric(stringr::str_sub(majorclass, 1, 2)),
           dim = dplyr::case_when(mc_1 == 1 ~ "local",
                           mc_1 == 2 ~ "other",
                           mc_1 == 3 ~ "state",
                           mc_1 == 4 ~ "fed.other",
                           mc_1 == 5 ~ "other",
                           mc_1 == 6 ~ "trans"),
           dim = ifelse(suffix %in% c(230, 250), "fed.stim", dim),
           dim = ifelse(mc_2 > 54 & mc_2 < 60, "trans", dim))%>% 
    dplyr::group_by(FY, dnum, dname, dim) %>%
    dplyr::summarise(amount = sum(amount, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(id_cols = c(FY, dnum), 
                names_from = dim, 
                values_from = amount, names_prefix = "fid.r.") %>% 
    replace(is.na(.), 0) %>% 
    dplyr::mutate(fid.r.fed = fid.r.fed.other + fid.r.fed.stim, 
           fid.r.total = fid.r.local + fid.r.other + fid.r.state + fid.r.fed + fid.r.trans,
           # fid.r.state = ifelse(dnum == 55120, fid.r.state + fid.r.trans, fid.r.state), ## Stephenson adjustment (because they do their finances strangely)
           fid.r.total.audit = fid.r.local + fid.r.other + fid.r.state + fid.r.fed
    )
  
  
}


