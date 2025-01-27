


# this is the file I use to make yearly tweaks when I know if the state aid status
  # report data isn't reporting all the appropriated funds. 



#' FA data est
#'
#'
#' @export
fa_dwork_year_changes <- function(df){

  {{df }} %>% 
    
    
    ## add things to est.1.rev.state.manual
    
#### 2025 ###########################################################################################################################
  
    mutate(
      est.1.rev.state.manual.97j   = ifelse(FY == 2025, lag(cy.a.8.777) * 0.042, 0),
      est.1.rev.state.manual.67f   = ifelse(FY == 2025, lag(cy.a.8.780), 0),
      est.1.rev.state.manual.99h   = ifelse(FY == 2025, lag(cy.a.8.260) *  0.810, 0),
      est.1.rev.state.manual.152a  = ifelse(FY == 2025, lag(cy.a.8.848) * 1, 0),
      est.1.rev.state.manual.27k   = ifelse(FY == 2025, lag(cy.a.8.857) *  0.111 , 0),
      est.1.rev.state.manual.99g   = ifelse(FY == 2025, lag(cy.a.8.779) * 1, 0),
      
      est.1.rev.state.manual.35m   = ifelse(FY == 2025, enroll.pct.state.1 * 70000000, 0),
      est.1.rev.state.manual.104h  = ifelse(FY == 2025, enroll.pct.state.1 * 11500000, 0),
      
      est.1.rev.state.manual = est.1.rev.state.manual.97j + est.1.rev.state.manual.67f +
        est.1.rev.state.manual.99h + est.1.rev.state.manual.152a + est.1.rev.state.manual.27k +
        est.1.rev.state.manual.99g + est.1.rev.state.manual.35m + est.1.rev.state.manual.104h, 
      
      
      est.1.rev.state.41 = ifelse(FY == 2025, lag(cy.a.8.250) *  1.262, est.1.rev.state.41),

      
    ) %>% 
  
  

    
#### 2024 ###########################################################################################################################
  
    mutate(

      est.1.rev.state.31aa =
        ifelse(FY == 2024,
               enroll.pct.state.1 * 310000000,
               est.1.rev.state.31aa),


      est.1.rev.state.35j =
        ifelse(FY == 2024,
               enroll.pct.state.1 * 140000000,
               0),


      # some of the districts have section 41 and some that definately will get it don't have it yet
      # so I just estimated if they got it last year but don't have a direct appropriation this year
      est.1.rev.state.41 =
        ifelse(FY == 2024 & est.1.rev.state.41 == 0,
               lag(cy.a.8.250) * (1 + (cy.d.pupilcnt.pct.chg / 100)) * 1.5,
               est.1.rev.state.41),



      # Manual

      est.1.rev.state.manual = ifelse(FY == 2024,
                                      est.1.rev.state.31aa +
                                        est.1.rev.state.35j,
                                      # est.1.rev.state.41,
                                      est.1.rev.state.manual),


    ) %>%
  

#### 2023 ###########################################################################################################################
  
  mutate(est.1.rev.state.mpsers.uaal = ifelse(FY == 2023, 
                                              est.1.rev.state.mpsers.uaal * 1.6, 
                                              est.1.rev.state.mpsers.uaal),
         
         est.1.rev.state.31aa = ifelse(FY == 2023,
                                       cy.d.1.pupilcnt * 100,
                                       est.1.rev.state.31aa),
         
         
         est.1.rev.state.97 = ifelse(FY == 2023,
                                     cy.d.1.pupilcnt * 100,
                                     est.1.rev.state.97),
         
         # they combined them in 2024 so I want to show them together
         est.8.rev.state.31aa = ifelse(FY == 2023,
                                       est.8.rev.state.31aa + est.8.rev.state.97,
                                       est.8.rev.state.31aa),
         
         est.1.rev.state.41 = ifelse(FY == 2023 & est.1.rev.state.41 == 0,
                                     lag(est.8.rev.state.41),
                                     est.1.rev.state.41),
         
         
         
         
         
         # Manual  
         
         est.1.rev.state.manual = ifelse(FY == 2023,
                                         est.1.rev.state.31aa + 
                                           est.1.rev.state.97 +
                                           est.1.rev.state.41, 
                                         est.1.rev.state.manual),
         
         
  ) %>% 

    
    mutate(est.1.rev.state.mpsers.uaal = ifelse(FY == 2023, 
                                                est.1.rev.state.mpsers.uaal * 1.6, 
                                                est.1.rev.state.mpsers.uaal),
           
           est.1.rev.state.31aa = ifelse(FY == 2023,
                                         cy.d.1.pupilcnt * 100,
                                         est.1.rev.state.31aa),
           
           
           est.1.rev.state.97 = ifelse(FY == 2023,
                                       cy.d.1.pupilcnt * 100,
                                       est.1.rev.state.97),
           
           # they combined them in 2024 so I want to show them together
           est.8.rev.state.31aa = ifelse(FY == 2023,
                                         est.8.rev.state.31aa + est.8.rev.state.97,
                                         est.8.rev.state.31aa),
           
           est.1.rev.state.41 = ifelse(FY == 2023 & est.1.rev.state.41 == 0,
                                       lag(est.8.rev.state.41),
                                       est.1.rev.state.41),
           
          
           
           # Manual  
           
           est.1.rev.state.manual = ifelse(FY == 2023,
                                           est.1.rev.state.31aa + 
                                             est.1.rev.state.97 +
                                             est.1.rev.state.41, 
                                           est.1.rev.state.manual)
           
           
    )
  
  
  
  
  
  
}





