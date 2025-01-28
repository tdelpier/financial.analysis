

#' FA data work end
#'
#'
#' @export
fa_dwork_end <- function(df){
  
  {{ df }} %>% 
  mutate(
    
    # found my problem. Where do I do state total? 
    
    # State Total 
    
    # est.1.rev.state.single.payments = 0,
    
    
    est.1.rev.state = 
      cy.a.1.000 + est.1.rev.state.manual - cy.a.1.9999 - cy.a.1.8888,
    
    est.1.rev.state.error = est.1.rev.state - fid.r.state, 
    est.1.rev.state.pct.act = (est.1.rev.state / fid.r.state) * 100,
    
    
    # Total Mechanical Model
    est.1.rev.total = est.1.rev.local + est.1.rev.state + est.1.rev.fed + est.1.rev.other,
    est.1.rev.total.xstim = est.1.rev.total - est.1.rev.fed.stim, 
    
    est.1.rev.total.xstim.error = est.1.rev.total.xstim -fid.r.total.audit,
    est.1.rev.total.xstim.pct.act = (est.1.rev.total.xstim /fid.r.total.audit) * 100,
    est.1.rev.total.error = est.1.rev.total - fid.r.total.audit,
    est.1.rev.total.pct.act = (est.1.rev.total / fid.r.total.audit) * 100,
    
    
    fid.r.total.audit.lag1 = lag(fid.r.total.audit),
    fid.r.total.audit.lag1.pct.act = (fid.r.total.audit.lag1 / fid.r.total.audit) * 100,
    
    db.orig.rev.pct.act = (db.original.total.revenue /fid.r.total.audit) * 100, 
    
  ) %>% 
  
    
    
#### Budget Table ###########################################################################################################################
  
  mutate(
    
    # Local
    
    tab.rev.local = ifelse(FY == fiscal.year, 
                           est.1.rev.local, 
                           fid.r.local),
    
    tab.rev.local.found = ifelse(FY == fiscal.year, 
                                 est.1.rev.local.found, 
                                 est.8.rev.local.found),
    
    tab.rev.local.other = ifelse(FY == fiscal.year, 
                                 est.1.rev.local.other, 
                                 est.8.rev.local.other),
    
    
    # State
    tab.rev.state = ifelse(FY == fiscal.year, est.1.rev.state, fid.r.state),
    
    
    # regularly listed cats
    tab.rev.state.found =        ifelse(FY == fiscal.year, est.1.rev.state.found, est.8.rev.state.found),
    tab.rev.state.sped =         ifelse(FY == fiscal.year, est.1.rev.state.sped, est.8.rev.state.sped),
    tab.rev.state.31a =          ifelse(FY == fiscal.year, est.1.rev.state.31a, est.8.rev.state.31a),
    tab.rev.state.mpsers.uaal =  ifelse(FY == fiscal.year, est.1.rev.state.mpsers.uaal, est.8.rev.state.mpsers.uaal),
    tab.rev.state.mspers.other =  ifelse(FY == fiscal.year, est.1.rev.state.mpsers.other, est.8.rev.state.mpsers.other),
    tab.rev.state.41 =           ifelse(FY == fiscal.year, est.1.rev.state.41, est.8.rev.state.41), ,

    
    # Changed in FY 2024
    tab.rev.state.31aa =         ifelse(FY == fiscal.year, est.1.rev.state.31aa, est.8.rev.state.31aa),
    # tab.rev.state.35j =          ifelse(FY == fiscal.year, est.1.rev.state.35j,est.8.rev.state.35j),
    tab.rev.state.29 =           ifelse(FY == fiscal.year, est.1.rev.state.29, est.8.rev.state.29),
    # tab.rev.state.27l =          ifelse(FY == fiscal.year, est.1.rev.state.27l, est.8.rev.state.27l),
    # tab.rev.state.meals =        ifelse(FY == fiscal.year, est.1.rev.state.meals, est.8.rev.state.meals),
    tab.rev.state.22l =          ifelse(FY == fiscal.year, est.1.rev.state.22l, est.8.rev.state.22l),

    
    # state other 
    est.rev.state.listed.regularly = tab.rev.state.found + tab.rev.state.sped + 
      tab.rev.state.31a + tab.rev.state.41 + tab.rev.state.mpsers.uaal,
    
    est.rev.state.listed.this.year = tab.rev.state.31aa  +
      tab.rev.state.29 + tab.rev.state.mspers.other + tab.rev.state.22l, 
    
    est.rev.state.listed = est.rev.state.listed.regularly + est.rev.state.listed.this.year, 
    
    tab.rev.state.other = tab.rev.state - est.rev.state.listed,
    
    
    
    # Federal
    tab.rev.fed = ifelse(FY == fiscal.year, est.1.rev.fed, fid.r.fed ),
    tab.rev.fed.stim =  ifelse(FY == fiscal.year, est.1.rev.fed.stim, fid.r.fed.stim),
    tab.rev.fed.other = ifelse(FY == fiscal.year, est.1.rev.fed.other, fid.r.fed.other),
    
    
    # Other
    tab.rev.other = ifelse(FY == fiscal.year, est.1.rev.other, fid.r.other),
    
    
    # Total
    tab.rev.total.xstim.xuaal = tab.rev.local + tab.rev.state + tab.rev.fed + tab.rev.other - tab.rev.fed.stim - tab.rev.state.mpsers.uaal,
    tab.rev.total.xstim = tab.rev.local + tab.rev.state + tab.rev.fed + tab.rev.other - tab.rev.fed.stim, 
    tab.rev.total = tab.rev.total.xstim + tab.rev.fed.stim
    
    
  ) 
  
  
  
  
  
  
  
}
