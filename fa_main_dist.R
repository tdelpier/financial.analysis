

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
 tt_dnum_from_dname("baldw")
 
 
dnum_x <- tt_dnum_random(1)
dnum_x <- 15010 # beaver island
# dnum_x <- 18010 # Clare
# dnum_x <- 82055 # Grosse Pointe 
# dnum_x <- 43040 # baldwin
dnum_x <- 52170 # Marquette
dnum_x <-  33020 # Lansing
dnum_x <- 76210 #sandusky
dnum_x <- 82090 # Lincoln Park
dnum_x <- 82340 # huron problem
dnum_x <- 39065 # gull lake
dnum_x <- 41050 # Caledonia        
dnum_x <- 50210 # Utica
dnum_x <- 70020 # holand
dnum_x <- 2070  # munising
dnum_x <- 25010 # flint
dnum_x <- 50070 # Clintondale
dnum_x <- 82160 # wayne westland
dnum_x <- 83010 # Cadillac
dnum_x <- 11010 # Benton Harbor
dnum_x <- 27010 # bessemer missing data
dnum_x <- 43040 # Baldwin weird grant

FA_Data_District <-
  FA_Data %>%
  filter(dnum == dnum_x ,
         FY >= (fiscal.year - 10)) %>%
  arrange(FY) %>%
  ungroup()


FA_Data %>% 
  fa_helper_render_district_fa(dnum_x,
                               type = "paged", # "web", "paged", or "slides"
                               page.details = TRUE,
                               page.revest = TRUE, 
                               page.budcomp = TRUE,
                               page.sam = TRUE,
                               page.found = TRUE,
                               page.revexp = TRUE,
                               page.gfb = TRUE,
                               page.transfers = TRUE,
                               page.millages = TRUE,
                               page.stutransfers = TRUE,
                               page.econdisad = TRUE,
                               page.methods = TRUE,
                               
                               # not using
                               page.stim = FALSE,
                               page.cac = FALSE, 
                               page.surplus = FALSE
                               
                               )


rm(FA_Data_District, districts, dnum_x, counter, total)

districts <- 
  district_id %>% 
  mutate(dnum = as.numeric(dcode)) %>% 
  tt_dnum_isd(dnum) %>% 
  tt_dnum_psa(dnum) %>% 
  filter(flag.charter == 0,
         flag.isd == 0) %>% 
  select(dnum) %>% 
  pull() 


# districts <- tt_dnum_random(10)

counter <-  0
total <- length(districts)

for (dnum_x in districts) {
  
  counter <- counter + 1
  message(paste0("Processing ", counter, " of ", total, " (District: ", dnum_x, ")"))
  
  fa_output_district_fa(dnum_x)
  
}


# There's a better way to do this
  # want to pull all the dcodes and make sure the data is correct before running


