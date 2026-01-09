

library(tidyverse)
library(TannersTools)
library(financial.analysis)
library(testthat)

# setup 
fiscal.year <- 2025
fa_setup()



# construct the data from scratch
fa_construct_data()
fa_import_data("raw") %>% fa_dwork()

# load the data
FA_Data <- fa_import_data("prepped")
dist_geo <- fa_import_geo() %>% rename(geometry = geom)

fund_names <- 
  TannersTools::FID_names_fund %>% 
  mutate(var.name = paste0("fid.b.fb.", fund))
recode_vec <- setNames(fund_names$var.name, fund_names$fund.name)


# Choosing District 
 tt_dnum_from_dname("kent")
 tt_dnum_from_dname("benzie")
 tt_dnum_from_dname("utica")
 
 
dnum_x <- tt_dnum_random(1)
# dnum_x <- 15010 # beaver island
# dnum_x <- 18010 # Clare
# dnum_x <- 82055 # Grosse Pointe 
# dnum_x <- 43040 # baldwin
dnum_x <- 52170 # Marquette
# dnum_x <-  33020 # Lansing
# dnum_x <- 18020
dnum_x <- 76210 #sandusky
dnum_x <- 82090 # Lincoln Park
dnum_x <- 82160
dnum_x <- 54025
dnum_x <- 82340 # huron problem
dnum_x <- 39065 # gull lake
dnum_x <- 25180
dnum_x <- 10015
dnum_x <- 41050 # Caledonia        
dnum_x <- 50210 # Utica


FA_Data_District <-  
  FA_Data %>%
  filter(dnum == dnum_x ,
         FY >= (fiscal.year - 10)) %>%
  arrange(FY) %>% 
  ungroup()


FA_Data_District %>% 
  fa_helper_render_district_fa(dnum_x,
                               type = "paged", # "web", "paged", or "slides"
                               page.details = FALSE,
                               page.revest = FALSE, 
                               page.budcomp = FALSE,
                               page.stim = FALSE,
                               page.sam = FALSE,
                               page.found = FALSE,
                               page.revexp = FALSE,
                               page.surplus = FALSE,
                               page.gfb = TRUE,
                               page.transfers = TRUE,
                               page.methods = FALSE,
                               
                               # not using
                               page.millages = FALSE,
                               page.cac = FALSE
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

