




#' FA helper variable manual add and manual subtract variables
#'
#' 
#'
#' @export
create_manual_add_sub <- function(df, FY, suffix, manual_add, manual_sub){
  
  new_var_manual_add <- paste0("manual.add.",  suffix )
  new_var_manual_sub <- paste0("manual.sub.",  suffix )

  df <- df %>% 
    bind_rows(tibble(!!new_var_manual_add := logical())) %>% 
    bind_rows(tibble(!!new_var_manual_sub := logical())) %>% 
    mutate("{new_var_manual_add}" := ifelse(FY == fiscal.year, {{ manual_add }}, .data[[new_var_manual_add]]),
           "{new_var_manual_sub}" := ifelse(FY == fiscal.year, {{ manual_sub }}, .data[[new_var_manual_sub]]))

  return(df)
  
}

# I have lines that automatically add up all "manual.add" and "manual.sub" lines.
# this function is to help me keep consistent with my additions and subtractions



# 
# x2 %>% 
#     filter(dnum == dnum_x,
#            FY > 2019) %>%
#   # mutate(manual.add.104h = 1) %>% 
#   mutate(cy.a.1.096 = 2) %>%
#   create_manual_add_sub(2026, "104h", manual_add = lag(cy.a.8.096), manual_sub =  3) %>% 
#   select(FY, starts_with("manual"), cy.a.8.096)
  
  
