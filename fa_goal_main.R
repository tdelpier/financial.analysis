

library(tidyverse)
library(TannersTools)
# library(scales)
# library(vtable)
# library(flextable)
# library(ggtext)
library(financial.analysis)
# library(servr)




fiscal.year <- 2024

fa_setup()
fa_construct_data()
fa_import_data("raw") %>% fa_dwork()

FA_Data <- fa_import_data("prepped")

dist_geo <- fa_import_geo() %>% rename(geometry = geom)

tt_dnum_from_dname("flint")

FA_Data_District <-  
  FA_Data %>%
  filter(dnum == 52170 ,
         FY > (fiscal.year - 10)) %>%
  arrange(FY) %>% 
  ungroup()

# marquette 52170
# flint 25010

FA_Data_District %>% 
  fa_helper_render_district_fa(25010,
                               type = "paged", # "web" or "paged"
                               page.details = FALSE,
                               page.revest = FALSE, 
                               page.budcomp = FALSE,
                               page.stim = FALSE,
                               page.sam = TRUE,
                               page.found = FALSE,
                               page.revexp = FALSE,
                               page.cac = FALSE,
                               page.gfb = FALSE,
                               page.transfers = FALSE,
                               page.methods = FALSE)

# I think I want to reorder: 
# 1. setup
# 2. import data
# 3. dwork
# 4. construct data
# 5. cache data
# 6. open data
# 7. render reports 


