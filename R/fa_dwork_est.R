


#' FA data est
#'
#'
#' @export
fa_dwork_est <- function(df){
  
  {{ df }} %>% 
    
    group_by(dnum) %>%
    arrange(dnum, FY) %>%
    
    mutate(
      
      
      # Local ########################################################################
      
      # foundation
      est.1.rev.local.found = cy.d.1.found.rev.local,
      est.8.rev.local.found = cy.d.8.found.rev.local,
      est.8.rev.local.other = fid.r.local - est.8.rev.local.found,
      est.8.rev.local.other = ifelse(est.8.rev.local.other > 0 , est.8.rev.local.other, 0),
      
      # local other
      est.1.rev.local.other = lag(est.8.rev.local.other),
      est.1.rev.local.other = ifelse(est.1.rev.local.other > 0 , est.1.rev.local.other, 0), 
      
      # total local
      est.1.rev.local = est.1.rev.local.found + est.1.rev.local.other,
      
      # errors
      est.1.rev.local.error = est.1.rev.local - fid.r.local, 
      est.1.rev.local.pct.act = (est.1.rev.local / fid.r.local) * 100,
      
      
      
      
      # State ######################################################################## 
      
      # foundation
      est.1.rev.state.found = cy.a.1.020 + cy.a.1.039 + cy.a.1.024 + cy.a.1.042,
      est.8.rev.state.found = cy.a.8.020 + cy.a.8.039 + cy.a.8.024 + cy.a.8.042,
      
      #sped
      est.1.rev.state.sped = cy.a.1.033 + cy.a.1.081 + cy.a.1.036, 
      est.8.rev.state.sped = cy.a.8.033 + cy.a.8.081 + cy.a.8.036, 
      
      # 31a - at risk
      est.1.rev.state.31a = cy.a.1.231,
      est.8.rev.state.31a = cy.a.8.231, 
      
      # 41 - bilingual
      est.1.rev.state.41 = cy.a.1.250,
      est.8.rev.state.41 = cy.a.8.250,
      
      # MPSERS UAAL
      est.1.rev.state.mpsers.uaal = cy.a.1.573 + cy.a.1.583,
      est.8.rev.state.mpsers.uaal = cy.a.8.573 + cy.a.8.583,
      
      # MPSERS Other
      # est.1.rev.state.mpsers.other = cy.a.1.572 + cy.a.1.079 + cy.a.1.082,
      # est.8.rev.state.mpsers.other = cy.a.8.572 + cy.a.8.079 + cy.a.8.082,
      
      
      
      ### Not automatically listed
      
      # 31aa - mental health
      est.1.rev.state.31aa = ifelse("cy.a.1.174" %in% names(.), cy.a.1.174, 0), # they've never distributed 31aa in January 
      est.8.rev.state.31aa = cy.a.8.174,
      
      # 97 - school safety 
      est.1.rev.state.97 = ifelse("cy.a.1.198" %in% names(.), cy.a.1.198, 0), # not distributed by January 
      est.8.rev.state.97 = cy.a.8.198,
      
      # 29 - enrollment stabilization
      est.1.rev.state.29 = cy.a.1.076,
      est.8.rev.state.29 = ifelse("cy.a.8.076" %in% names(.), cy.a.8.076, 0),
      
      # 35j - early literacy
      est.1.rev.state.35j = 0,
      est.8.rev.state.35j = 0,
      
      # 22l - Transportation 
      est.1.rev.state.22l = cy.a.1.106, 
      est.8.rev.state.22l = ifelse("cy.a.8.106" %in% names(.), cy.a.8.106, 0),
      
      # Universal Meals
      est.1.rev.state.meals = cy.a.1.235 + cy.a.1.335 + cy.a.1.354,
      est.8.rev.state.meals = ifelse(FY > 2023 & cy.a.8.000 != 0, 
                                     cy.a.8.235 + cy.a.8.335 + cy.a.8.354,
                                     cy.a.8.235),
      
      # 27l Educator Compensation
      est.1.rev.state.27l = cy.a.1.858, 
      est.8.rev.state.27l = ifelse("cy.a.8.858" %in% names(.), cy.a.8.858, 0),
      
      # 147f - 0.5% Payroll reimbursement 
      est.1.rev.state.147f = cy.a.1.587,
      est.8.rev.state.147f = ifelse("cy.a.8.587" %in% names(.), cy.a.8.587, 0),
      
      # state manual blank
      est.1.rev.state.manual = 0, 
      
      
      
      
      # Federal ######################################################################
      est.1.rev.fed.other = lag(fid.r.fed.other),
      est.1.rev.fed.stim =  ifelse(FY > 2020 & FY < 2026, 
                                   ((stim.alloc.total - fid.e.fed.stim.sum.total)),
                                   0),
      est.1.rev.fed.stim = ifelse(est.1.rev.fed.stim < 0, 0, est.1.rev.fed.stim),
      est.1.rev.fed = est.1.rev.fed.other + est.1.rev.fed.stim, 
      
      est.1.rev.fed.error = est.1.rev.fed - fid.r.fed,
      est.1.rev.fed.pct.act = (est.1.rev.fed / fid.r.fed) * 100,
      
      
      # Other 
      # est.1.rev.other =  lag(rollapplyr(fid.r.other, 5, mean, partial = TRUE)),
      est.1.rev.other = lag(fid.r.other),
      
      est.1.rev.other.error = est.1.rev.other - fid.r.other,
      est.1.rev.other.pct.act = est.1.rev.other / fid.r.other
      
    )
  
}
