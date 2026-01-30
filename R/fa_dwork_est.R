


#' FA data est
#'
#'
#' @export
fa_dwork_est <- function(df){
  
  
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
    create_tabrev_var_from_icd(var_name = "tab.rev.state.cte",             c("470", "088", "490")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.edcomp",          c("cy.adj.1.858")) %>%  
    
    create_tabrev_var_from_icd(var_name = "tab.rev.state.mpsers.uaal" ,    c("573", "583", "598")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.mpsers.netother", c("597", "cy.adj.1.567")) %>% 
    create_tabrev_var_from_icd(var_name = "tab.rev.state.mpsers.other" ,   c("079", "082", "572", "588", "587", "cy.adj.1.079")) %>% 
    
    mutate(
      tab.rev.state.mpsers.other = 
        case_when(FY == 2026 ~
                    ((lag(cy.a.8.079) * 0.92) #147a(2)
                     +(lag(cy.a.8.082) * 1.13) #147e
                     + cy.a.1.572 #147a(1)
                     + cy.a.1.588 + #147a(3)
                       + cy.adj.1.079  #147a(2) additional $50 million allocation adjustment for FY25
                     + cy.a.1.587),
                  .default = tab.rev.state.mpsers.other)) %>% 
    
    
    create_tabrev_var_from_icd(var_name = "in.jan", c("037", "046", "064", "171", "175", "228", "251", "666", "848", "1023")) %>% # things to keep unchanged and aggregated
    create_tabrev_var_from_icd(var_name = "onetime.safsr", c("1023") ) %>% 
    create_tabrev_var_from_icd(var_name = "non.gf.safsr.rev" ,   c("235", "335", "354")) %>% 
    
    mutate(manual = 
             lag(cy.a.8.096) +  # 104h
             (lag(cy.a.8.865) * 0.8) + #35m
             lag(cy.a.8.260),  #99h
           
           rev.state.safsr.fid.error = (fid.r.state - onetime.safsr) - (cy.a.8.000 - non.gf.safsr.rev), 
           listed.counted.rev.state = rowSums(across(any_of(starts_with("tab.rev.state."))), na.rm = TRUE),
           tab.rev.state.other = ifelse(FY != fiscal.year,  fid.r.state - listed.counted.rev.state, NA),
           tab.rev.state.other = ifelse(FY == fiscal.year, 
                                        in.jan + manual + lag(rev.state.safsr.fid.error), 
                                        tab.rev.state.other),
           tab.rev.state = rowSums(across(any_of(starts_with("tab.rev.state."))), na.rm = TRUE)
    ) %>%  
    
    
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
      
    ) %>% 
    
    
    mutate(
      
      fid.r.total.audit.lag1 = lag(fid.r.total.audit),
      fid.r.total.audit.lag1.pct.act = (fid.r.total.audit.lag1 / fid.r.total.audit) * 100,
      
      db.orig.rev.pct.act = (db.original.total.revenue /fid.r.total.audit) * 100, 
      db.orig.rev.xstim.pct.act = (db.original.total.revenue / (fid.r.total.audit - fid.r.fed.stim) * 100)
      
    )
  
  
  
}
