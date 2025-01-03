

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

tt_dnum_from_dname("marq")

FA_Data_District <-  
  FA_Data %>%
  filter(dnum == 52170 ,
         FY > (fiscal.year - 10)) %>%
  arrange(FY) %>% 
  ungroup()


# line_size = 1.2

FA_Data_District %>% 
  fa_helper_render_district_fa(52170,
                               type = "paged", # "web" or "paged"
                               page.details = FALSE,
                               page.revest = TRUE, 
                               page.budcomp = TRUE,
                               page.stim = TRUE,
                               page.sam = TRUE,
                               page.found = TRUE,
                               page.revexp = TRUE,
                               page.cac = TRUE,
                               page.gfb = TRUE,
                               page.transfers = FALSE,
                               page.methods = TRUE)

# I think I want to reorder: 
# 1. setup
# 2. import data
# 3. dwork
# 4. construct data
# 5. cache data
# 6. open data
# 7. render reports 

