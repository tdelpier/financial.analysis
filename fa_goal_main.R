

library(tidyverse)
library(TannersTools)
# library(scales)
# library(vtable)
# library(flextable)
# library(ggtext)
library(financial.analysis)
# library(servr)

# setup 
fiscal.year <- 2024
fa_setup()


# construct the data from scratch
fa_construct_data()
fa_import_data("raw") %>% fa_dwork()


# load the data
FA_Data <- fa_import_data("prepped")
dist_geo <- fa_import_geo() %>% rename(geometry = geom)


# Choosing District 
  # tt_dnum_from_dname("pont")
  # marquette 52170
  # flint 25010
  # ann arbor 81010
  # beaver island 15010
  # pontiac 63030

dnum_x <- tt_dnum_random(1)

FA_Data_District <-  
  FA_Data %>%
  filter(dnum == dnum_x ,
         FY > (fiscal.year - 10)) %>%
  arrange(FY) %>% 
  ungroup()


FA_Data_District %>% 
  fa_helper_render_district_fa(dnum_x,
                               type = "web", # "web" or "paged"
                               page.details = FALSE,
                               page.revest = FALSE, 
                               page.budcomp = FALSE,
                               page.stim = FALSE,
                               page.sam = FALSE,
                               page.found = FALSE,
                               page.revexp = FALSE,
                               page.surplus = FALSE, 
                               page.cac = FALSE,
                               page.gfb = FALSE,
                               page.transfers = FALSE,
                               page.milages = TRUE,
                               page.methods = FALSE)

# I think I want to reorder: 
# 1. setup
# 2. import data
# 3. dwork
# 4. construct data
# 5. cache data
# 6. open data
# 7. render reports 


