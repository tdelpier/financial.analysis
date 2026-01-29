



#' FA helper variable to create tab.rev variables 
#'
#' 
#'
#' @export
create_tabrev_var_from_est <- function(df, var) {
  
  df %>%
    mutate("tab.rev.{var}" :=
             ifelse(FY == fiscal.year,
                    .data[[paste0("est.1.rev.", {{ var }})]],
                    .data[[paste0("est.8.rev.", {{var }})]]))
  
}

### and this is how to use it ... 
# rev_est_variable_suffixes <- c("state.found", "state.sped")
# df_1 <- FA_Data_District %>% select(FY, est.1.rev.state.found, est.8.rev.state.found, est.1.rev.state.sped, est.8.rev.state.sped)
# df_1 %>% reduce(.x = rev_est_variable_suffixes, .f = ~ create_tabrev_var_from_est(.x, .y), .init = .) %>% view()



 