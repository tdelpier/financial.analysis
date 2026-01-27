


#' FA data work end
#'
#'
#' @export
fa_dwork_end <- function(df){
  
  
  rev_est_variable_suffixes <- 
    c("local.found", "local.other",
      "state.found", "state.sped", "state.31a", "state.41", 
      "state.31aa", "state.29", "state.22l", "state.cte", "state.edcomp",
      "state.mpsers.uaal", "state.mpsers.netother", "state.mpsers.other")

  
  {{ df }} %>% 
  mutate(
    
    # found my problem. Where do I do state total? 
    
    # State Total 
    
    # est.1.rev.state.single.payments = 0,
    
    
    fid.r.state.to.cy.a.8.000.error = fid.r.state - (cy.a.8.000 - est.8.rev.state.meals),
    # fid.r.state.to.cy.a.8.000.error = ifelse(x < 0, 0, x),
    # error between august state payment and actual recieved 
    
    est.1.rev.state = 
      cy.a.1.000 # start with total January SASR 
    + est.1.rev.state.manual # add manual cat and adjustments 
    + lag(fid.r.state.to.cy.a.8.000.error) # add FID to August SASR error from last year
    - est.1.rev.state.meals # school meals revenue don't go to the general fund
    - cy.a.1.9999 - cy.a.1.8888, # subtracting out any deductions
    
    est.1.rev.state.error = est.1.rev.state - fid.r.state, 
    est.1.rev.state.pct.act = (est.1.rev.state / fid.r.state) * 100,
    est.8.rev.state.pct.act = (cy.a.8.000 / fid.r.state) * 100,
    
    # Total Mechanical Model
    est.1.rev.total = est.1.rev.local + est.1.rev.state + est.1.rev.fed + est.1.rev.other,
    est.1.rev.total.xstim = est.1.rev.total - est.1.rev.fed.stim, 
    
    est.1.rev.total.xstim.error = est.1.rev.total.xstim -fid.r.total.audit,
    est.1.rev.total.xstim.pct.act = (est.1.rev.total.xstim / (fid.r.total.audit - fid.r.fed.stim)) * 100,
    est.1.rev.total.error = est.1.rev.total - fid.r.total.audit,
    est.1.rev.total.pct.act = (est.1.rev.total / fid.r.total.audit) * 100,
    
    
    fid.r.total.audit.lag1 = lag(fid.r.total.audit),
    fid.r.total.audit.lag1.pct.act = (fid.r.total.audit.lag1 / fid.r.total.audit) * 100,
    
    db.orig.rev.pct.act = (db.original.total.revenue /fid.r.total.audit) * 100, 
    db.orig.rev.xstim.pct.act = (db.original.total.revenue / (fid.r.total.audit - fid.r.fed.stim) * 100)
    
    
  ) %>%
    
  
    
    
#### Budget Table ###########################################################################################################################
  
  mutate(
    
  
    tab.rev.local = ifelse(FY == fiscal.year,
                           est.1.rev.local,
                           fid.r.local),
 
    tab.rev.state = ifelse(FY == fiscal.year, est.1.rev.state, fid.r.state)) %>% 
    
    # this takes everything in the the suffix variable and creates a tab.rev. variable 
    reduce(.x = rev_est_variable_suffixes, .f = ~ create_tabrev_var_from_est(.x, .y), .init = .) %>% 
  
    
    # state other 
    mutate(
    est.rev.state.listed.regularly = 
      tab.rev.state.found + 
      tab.rev.state.sped + 
      tab.rev.state.31a + 
      tab.rev.state.41,
    
    est.rev.state.listed.this.year = 
      tab.rev.state.31aa  +
      tab.rev.state.29  + 
      tab.rev.state.22l + 
      tab.rev.state.cte + 
      tab.rev.state.edcomp +
      tab.rev.state.mpsers.uaal + 
      tab.rev.state.mpsers.netother + 
      tab.rev.state.mpsers.other, 
    
    est.rev.state.listed = est.rev.state.listed.regularly + est.rev.state.listed.this.year, 
    
    tab.rev.state.other = tab.rev.state - est.rev.state.listed,
    
    
    
    # Federal
    tab.rev.fed = ifelse(FY == fiscal.year, est.1.rev.fed, fid.r.fed ),
    tab.rev.fed.stim =  ifelse(FY == fiscal.year, est.1.rev.fed.stim, fid.r.fed.stim),
    tab.rev.fed.other = ifelse(FY == fiscal.year, est.1.rev.fed.other, fid.r.fed.other),
    
    
    # Other
    tab.rev.other = ifelse(FY == fiscal.year, est.1.rev.other, fid.r.other),
    
    
    # Total
    
    tab.rev.total = tab.rev.local + tab.rev.state + tab.rev.fed + tab.rev.other, 
      # tab.rev.total.xstim = tab.rev.total - tab.rev.fed.stim, 
      # tab.rev.total.xstim.xuaal = tab.rev.total - tab.rev.fed.stim - tab.rev.state.mpsers.uaal,
    
    tab.rev.net.zero.and.stim = tab.rev.state.edcomp + tab.rev.state.mpsers.uaal + tab.rev.state.mpsers.netother + tab.rev.fed.stim,
    tab.rev.total.xstim.xnet = tab.rev.total - tab.rev.net.zero.and.stim, 
    
    
    
    
    
  ) 
  
  
  
  
  
  
  
}
