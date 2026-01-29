
fun_est_temp <- function(df){
  
  {{ df }} %>%
    
  # Local

    mutate(
      tab.rev.local.found = ifelse(FY == fiscal.year, cy.d.1.found.rev.local, cy.d.8.found.rev.local),
      tab.rev.local.other = ifelse(FY == fiscal.year,
                                   pmax(lag(fid.r.local - cy.d.8.found.rev.local), 0 ), 
                                   pmax(fid.r.local - cy.d.8.found.rev.local, 0)),
      tab.rev.local = ifelse(FY == fiscal.year,
                             cy.d.1.found.rev.local + tab.rev.local.other,
                             fid.r.local),
    ) %>% 
  
  # State 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.found",           c("020", "039", "024", "042")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.sped",            c("033", "081", "036", "380", "430", "440")) %>%
    create_tabrev_var_from_icd(var_name = "tab.rev.state.31a",             c("231")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.41"  ,            c("250")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.31aa" ,           c("174")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.29",              c("076")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.22l"  ,           c("106")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.cte",             c("470", "088")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.edcomp",          c("cy.adj.1.858")) %>%  
    
    create_tabrev_var_from_icd(var_name = "tab.rev.state.mpsers.uaal" ,    c("573", "583", "598")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.mpsers.netother", c("597", "cy.adj.1.567")) %>% 
    
    create_tabrev_var_from_icd(var_name = "tab.rev.state.mspers.other" ,   c("079", "082", "572", "588", "587", "cy.adj.1.079")) %>% 
 
##########################################################################################################################
##################################### this is the chunk I'm working on 
##########################################################################################################################
    
  create_tabrev_var_from_icd(rev.state.other.1 ) # things to keep unchanged and aggregated
  muate(rev.state.other.2 = things to change, 
        tab.rev.state.other = rev.state.other.1  + rev.state.other.2)
  
  
  
  # Manual 
    mutate(
      tab.rev.state.mspers.other = 
        case_when(FY == 2026 ~
                    ((lag(cy.a.8.079) * 0.92) #147a(2)
                     +(lag(cy.a.8.082) * 1.13) #147e
                     + cy.a.1.572 #147a(1)
                     + cy.a.1.588 + #147a(3)
                     + cy.adj.1.079  #147a(2) additional $50 million allocation adjustment for FY25
                     + cy.a.1.587),
                  .default = tab.rev.state.mspers.other)) %>% 
    
    create_manual_add_sub(FY = 2026, suffix = "104h", manual_add = lag(cy.a.8.096), manual_sub = 0) %>% 
    create_manual_add_sub(FY = 2026, suffix = "35m", manual_add = lag(cy.a.8.865) * 0.8, manual_sub = cy.a.1.865) %>% 
    # create_manual_add_sub(FY = 2026, suffix = "147a2", manual_add = ((lag(cy.a.8.079) * 0.92)) , manual_sub = 0) %>%
    # create_manual_add_sub(FY = 2026, suffix = "147e", manual_add = (lag(cy.a.8.082) * 1.13), manual_sub = 0) %>%
    # create_manual_add_sub(FY = 2026, suffix = "147a2add", manual_add = cy.adj.1.079, manual_sub = 0) %>%

      
    create_tabrev_var_from_icd(var_name = "uncounted.rev.state" ,   c("235", "335", "354")) %>% 
    mutate(listed.counted.rev.state = rowSums(across(any_of(starts_with("tab.rev.state."))), na.rm = TRUE),
           manual.sub = rowSums(across(any_of(starts_with("manual.sub."))), na.rm = TRUE),
           manual.add = rowSums(across(any_of(starts_with("manual.add."))), na.rm = TRUE),
           manual.change = manual.add - manual.sub, 
           safsr.fidr.error = fid.r.state - listed.counted.rev.state, 
           
           tab.rev.state = ifelse(FY != fiscal.year, fid.r.state, NA),
           
           tab.rev.state.other = ifelse(FY != fiscal.year,  tab.rev.state - listed.counted.rev.state, NA),
           tab.rev.state.other = ifelse(FY == 2026, 
                                        manual.add.35m + manual.add.104h + cy.a.1.848  + lag(safsr.fidr.error),
                                        tab.rev.state.other),
           
           
           # tab.rev.state.other = ifelse(FY == fiscal.year,
           #                              cy.a.1.000 
           #                              - listed.counted.rev.state 
           #                              + manual.change 
           #                              + lag(safsr.fidr.error)
           #                              # - uncounted.rev.state
           #                              - cy.a.1.9999 - cy.a.1.8888,
           #                              tab.rev.state - listed.counted.rev.state), 
           # need to subtract out this - cy.a.1.9999 - cy.a.1.8888,
           
           tab.rev.state = ifelse(FY == fiscal.year, 
                                  rowSums(across(any_of(starts_with("tab.rev.state."))), na.rm = TRUE),
                                  tab.rev.state)) %>% 

    
  #   + lag(fid.r.state.to.cy.a.8.000.error)
  # 
  # 
  # fid.r.state.to.cy.a.8.000.error = fid.r.state - (cy.a.8.000 - est.8.rev.state.meals),
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

  # Federal
    
    mutate(
      est.1.rev.fed.other = lag(fid.r.fed.other),
      est.1.rev.fed.stim =  ifelse(FY > 2020 & FY < 2026, 
                                   ((stim.alloc.total - fid.e.fed.stim.sum.total)),
                                   0),
      est.1.rev.fed.stim = ifelse(est.1.rev.fed.stim < 0, 0, est.1.rev.fed.stim),
      est.1.rev.fed = est.1.rev.fed.other + est.1.rev.fed.stim, 
      
      est.1.rev.fed.error = est.1.rev.fed - fid.r.fed,
      est.1.rev.fed.pct.act = (est.1.rev.fed / fid.r.fed) * 100,
      
      
      tab.rev.fed = ifelse(FY == fiscal.year, 
                           est.1.rev.fed, 
                           fid.r.fed ),
      tab.rev.fed.stim =  ifelse(FY == fiscal.year, est.1.rev.fed.stim, fid.r.fed.stim),
      tab.rev.fed.other = ifelse(FY == fiscal.year, est.1.rev.fed.other, fid.r.fed.other)
      
    ) %>% 
   
  # Other 
    mutate(
      est.1.rev.other = lag(fid.r.other),
      est.1.rev.other.error = est.1.rev.other - fid.r.other,
      est.1.rev.other.pct.act = (est.1.rev.other / fid.r.other) * 100, 
      tab.rev.other = ifelse(FY == fiscal.year, est.1.rev.other, fid.r.other),

    ) %>% 
  
  # Totals
    mutate(
      tab.rev.total = tab.rev.local + tab.rev.state + tab.rev.fed + tab.rev.other, 
      tab.rev.net.zero.and.stim = tab.rev.state.edcomp + tab.rev.state.mpsers.uaal + tab.rev.state.mpsers.netother + tab.rev.fed.stim,
      tab.rev.total.xstim.xnet = tab.rev.total - tab.rev.net.zero.and.stim, 
      
    )
    
    
  
}




# mutate(
#   unlisted.counted.rev.state.104h = ifelse(FY == 2026, lag(cy.a.8.096), 0),
#   unlisted.counted.rev.state.35m = ifelse(FY == 2026, lag(cy.a.8.865) * 0.8, 0),
#   
#   unidentified.counted.rev.state = cy.a.1.000 - uncounted.rev.state + unlisted.counted.rev.state + listed.counted.rev.state
# ) %>% 
# create_tabrev_var_from_icd(var_name = "unlisted.counted.other", c()) %>% 
# 
# create_tabrev_var_from_icd(var_name = "uncounted.rev.state" ,   c("235", "335", "354")) %>% 
# 
# mutate(
#   unlisted.counted.rev.state = rowSums(across(any_of(starts_with("unlisted.counted."))),
#   listed.counted.rev.state = rowSums(across(any_of(starts_with("tab.rev.state."))), na.rm = TRUE),
# 
#   
#   
#   tab.rev.state.other = ifelse(FY == fiscal.year, 
#                                xx1, # what to put here is probably one of my biggest challenges 
#                                
#                                fid.r.state - listed.counted.rev.state),  
#   
#   tab.rev.state = listed.counted.rev.state + unlisted.counted.rev.state, 
#   # tab.rev.state = rowSums(across(any_of(starts_with("tab.rev.state."))), na.rm = TRUE)
#   
# ) %>% 

