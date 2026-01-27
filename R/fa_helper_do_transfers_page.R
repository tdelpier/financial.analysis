



#' FA Render do transfer page?
#'
#' Helps the render function understand wheter to run the transfer page or the alt t ransfer page. 
#'
#' @export
fa_helper_do_transfers_page <- function(fa_data_district, page.transfer = TRUE) {
  
  if(page.transfer == FALSE) {
    do_transfers_page <<- FALSE
    do_transfers_alt_page <<- FALSE
  } else{
    
    transfer_amount <- 
      fa_data_district %>% 
      select(starts_with("fid.trans.transfer.to.")) %>% 
      select(-fid.trans.transfer.to.fund.25) %>% 
      pivot_longer(cols = everything()) %>%
      summarize(value = sum(value, na.rm = TRUE)) %>% 
      pull()
    
    
    if(transfer_amount > 1000) {
      do_transfers_page <<- TRUE
      do_transfers_alt_page <<- FALSE
    } else{
      do_transfers_page <<- FALSE
      do_transfers_alt_page <<- TRUE
    }
    
  }
  
}
