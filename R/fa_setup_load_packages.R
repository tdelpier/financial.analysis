

#' FA Load Packages
#'
#'
#' @export
fa_setup_load_packages <- function() {
  
  packages = c("tidyverse",
               "lubridate", 
               "readr", 
               "vroom", 
               "purrr",
               "readxl",
               "scales",
               "zoo",
               "flextable",
               "janitor",
               "pagedown",
               "knitr",
               "sass",
               "extrafont",
               "tidymodels",
               "TannersTools",
               "sf",
               "cowplot",
               "ggsflabel",
               "smoothr",
               "lwgeom", 
               "formattable",
               "tools",
               "renv",
               "ggrepel",
               "ftExtra",
               "vtable",
               "ggtext"
               
  )
  
  
  ## Now load or install&load all
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )
  
  
  
  
}



