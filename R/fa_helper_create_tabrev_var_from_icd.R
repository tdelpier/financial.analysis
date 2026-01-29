

#' FA helper variable to create tab.rev variables from icd codes
#'
#' 
#'
#' @export
create_tabrev_var_from_icd <- function(df, var_name, ...) {
  
  needs_prefix <- grepl("^[0-9]", ...)
  jan_data <- ifelse(needs_prefix, paste0("cy.a.1.", ...), ...)
  aug_data <- ifelse(needs_prefix, paste0("cy.a.8.", ...), ...)
  
  df %>%
    mutate("{var_name}" := ifelse(FY == fiscal.year, # uses january data if current fiscal year
                                  rowSums(across(any_of(jan_data)), na.rm = TRUE),
                                  rowSums(across(any_of(aug_data)), na.rm = TRUE)
    ))
  
}


## how to use: 
# FA_Data %>% create_tabrev_var_from_icd(var_name = "tab.rev.xx", c("020", "039"))


# x2 %>% 
#   filter(dnum == dnum_x,
#          FY > 2019) %>% 
#   create_tabrev_var_from_icd(var_name = "tab.rev.xx", c("020", "039")) %>% 
#   select(FY, starts_with("tab.rev."), ends_with(c("020", "039", "567"))) 
# 
# 
# x2 %>% 
#   filter(dnum == dnum_x,
#          FY > 2019) %>% 
#   create_tabrev_var_from_icd(var_name = "tab.rev.xx", c("020", "039", "cy.adj.1.567")) %>% 
#   select(FY, tab.rev.xx, ends_with(c("020", "039", "567"))) 

