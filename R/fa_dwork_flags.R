


#' Creates summary flags to use elsewhere 
#'
#'
#'
#' @export
fa_dwork_flags <- function(df) {
  
  
  df %>%
  mutate(district.sqmi =  as.character(format(round(district.sqmi, 1),
                                              big.mark = ",")),
         cy.d.1.dname = tools::toTitleCase(tolower(cy.d.1.dname)),
         cy.d.pupilcnt.chg.1 = as.character(comma(cy.d.pupilcnt - cy.d.pupilcnt.x1, digits = 0)),
         cy.d.pupilcnt.chg.5 = as.character(comma(cy.d.pupilcnt - cy.d.pupilcnt.x5, digits = 0)),
         
         cy.d.pupilcnt.pct.chg.1 = ifelse(cy.d.pupilcnt.pct.chg.1 > 0,
                                          paste0("+", percent(cy.d.pupilcnt.pct.chg.1)),
                                          paste0(percent(cy.d.pupilcnt.pct.chg.1))),
         
         cy.d.pupilcnt.pct.chg.5 = ifelse(cy.d.pupilcnt.pct.chg.5 > 0,
                                          paste0("+", percent(cy.d.pupilcnt.pct.chg.5)),
                                          paste0(percent(cy.d.pupilcnt.pct.chg.5))),
         
         enroll.chg.1 = paste0(cy.d.pupilcnt.chg.1, " (", cy.d.pupilcnt.pct.chg.1,")" ),
         enroll.chg.5 = paste0(cy.d.pupilcnt.chg.5, " (", cy.d.pupilcnt.pct.chg.5,")" ),
         flag.out.of.formula = ifelse(cy.d.1.revenuepp > cy.d.1.found.pp, "Yes", "No"),
         flag.hh = ifelse(flag.hh == 1, "Yes", "No"))
  
  
  
}








