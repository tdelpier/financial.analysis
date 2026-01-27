

library(tidyverse)
library(TannersTools)
library(financial.analysis)
library(testthat)

# setup 
fiscal.year <- 2026
fa_setup()

# construct the data from scratch and store as "raw" in data directory
fa_construct_data()

# imports the "raw" data and perfomrs dwork on it. 
  # Then dumps that data into "prepped" in the data directory
fa_import_data("raw") %>% fa_dwork()


# load the most recent "prepped" data
FA_Data <- fa_import_data("prepped")


# loads the district geography data
dist_geo <- fa_import_geo() %>% rename(geometry = geom)



# Choosing District 
 tt_dnum_from_dname("kent")
 tt_dnum_from_dname("benzie")
 tt_dnum_from_dname("wayne")
 
 
dnum_x <- tt_dnum_random(1)
dnum_x <- 15010 # beaver island
# dnum_x <- 18010 # Clare
# dnum_x <- 82055 # Grosse Pointe 
# dnum_x <- 43040 # baldwin
dnum_x <- 52170 # Marquette
# dnum_x <-  33020 # Lansing
dnum_x <- 76210 #sandusky
dnum_x <- 82090 # Lincoln Park
dnum_x <- 82340 # huron problem
dnum_x <- 39065 # gull lake
dnum_x <- 41050 # Caledonia        
dnum_x <- 50210 # Utica
dnum_x <- 70020 # holand
dnum_x <- 2070 # munising
dnum_x <- 25010 # flint
dnum_x <- 50070 # Clintondale
dnum_x <- 82160 # wayne westland

FA_Data_District <-  
  FA_Data %>%
  filter(dnum == dnum_x ,
         FY >= (fiscal.year - 10)) %>%
  arrange(FY) %>% 
  ungroup()




FA_Data_District %>% 
  fa_helper_render_district_fa(dnum_x,
                               type = "web", # "web", "paged", or "slides"
                               page.details = FALSE,
                               page.revest = TRUE, 
                               page.budcomp = TRUE,
                               page.sam = FALSE,
                               page.found = FALSE,
                               page.revexp = FALSE,
                               page.gfb = FALSE,
                               page.transfers = FALSE,
                               page.millages = FALSE,
                               page.stutransfers = FALSE,
                               page.econdisad = FALSE,
                               page.methods = FALSE,
                               
                               # not using
                               page.stim = FALSE,
                               page.cac = FALSE, 
                               page.surplus = FALSE
                               
                               )



fa_output_district_fa(dnum_x)


# There's a better way to do this
  # want to pull all the dcodes and make sure the data is correct before running
dnum_x <- 
  district_id %>% 
  mutate(dnum = as.numeric(dcode)) %>% 
  tt_dnum_isd(dnum) %>% 
  tt_dnum_psa(dnum) %>% 
  filter(flag.charter == 0,
         flag.isd == 0) %>% 
  select(dnum) %>% pull()

