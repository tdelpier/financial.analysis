

library(tidyverse)
library(TannersTools)
library(scales)
library(vtable)
library(financial.analysis)


# this is the goal
fa_setup()
fa_construct_data()
FA_Data <- fa_import_data()
# we made it here! 

FA_dwork()
FA_render_dist()