


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
      
  # cats not included in jan 2025 SASR but in old budgets
      est.1.rev.state.manual.97j   = ifelse(FY == 2025, lag(cy.a.8.777) * 0.042, 0),
      est.1.rev.state.manual.67f   = ifelse(FY == 2025, lag(cy.a.8.780), 0),
      est.1.rev.state.manual.99h   = ifelse(FY == 2025, lag(cy.a.8.260) *  0.810, 0),
      est.1.rev.state.manual.152a  = ifelse(FY == 2025, lag(cy.a.8.848) * 1, 0),
      est.1.rev.state.manual.27k   = ifelse(FY == 2025, lag(cy.a.8.857) *  0.111 , 0),
      est.1.rev.state.manual.99g   = ifelse(FY == 2025, lag(cy.a.8.779) * 1, 0),
      
  # important cats new to FY 2025 budget but not in SASR
      est.1.rev.state.manual.35m   = ifelse(FY == 2025, enroll.pct.state.1 * 70000000, 0),
      est.1.rev.state.manual.104h  = ifelse(FY == 2025, enroll.pct.state.1 * 11500000, 0),
      
      
  # chaning incorrectly calculated cats in Jan SASR
      
      est.1.rev.state.41 = ifelse(FY == 2025, lag(cy.a.8.250) *  1.262, est.1.rev.state.41),
      # Jan 2025 SASR doesn't reflect increase in FY 2025 ed budget
      
      est.1.rev.state.31aa = ifelse(FY == 2025, cy.a.1.174 * 6.717, cy.a.1.174),
      est.8.rev.state.31aa = cy.a.8.174,
        # 31aa was listed in jan SASR but reflective of original ed budget with 92% cut from FY 2024
        # september 2024 supplemental budget added $150 million to sec. 31aa. 
        # Should be cut of aprox 54% w/ no enrollment change  
      
  
      est.1.rev.state.mpsers.other = lag(cy.a.8.079) + lag(cy.a.8.082) + cy.a.1.572 + cy.a.1.598 + cy.a.1.588 + cy.a.1.587,
        # used a lag of august 147a(2) and 147e because there were no significant change to appropriation 
        # jan 2025 SASR only includes less 40% of the total appropriation 
      

      est.1.rev.state.meals = est.8.rev.state.meals, 
      # increase in state budget to school meals but Jan 2025 SASR shows big decrease

      
      # total manaual adjustments
      est.1.rev.state.manual = est.1.rev.state.manual.97j + est.1.rev.state.manual.67f +
        est.1.rev.state.manual.99h + est.1.rev.state.manual.152a + est.1.rev.state.manual.27k +
        est.1.rev.state.manual.99g + est.1.rev.state.manual.35m + est.1.rev.state.manual.104h
      - cy.a.1.174 + est.1.rev.state.31aa # subtract listed 31aa and add calculated 31aa
      - cy.a.8.250 + est.1.rev.state.41  # subtract listed 41 and add calculated 41 
      - cy.a.1.235 - cy.a.1.335 - cy.a.1.354 + est.1.rev.state.meals # replacing school meals because it's not all come in
      - cy.a.1.079 + lag(cy.a.8.079) #147a(2) not fully included in jan 2025 SASR\
      - cy.a.1.082 + lag(cy.a.8.082) # same 147e
      
      
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





