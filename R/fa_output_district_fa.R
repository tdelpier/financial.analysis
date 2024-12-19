



TannersTools::tt_dir_projects("financial.analysis", "R", "fa_output_dist_fa_template.Rmd")
  

#' FA Render one district financial analysis
#'
#' 
#'
#' @export
fa_helper_render_district_fa <- function(df, dcode, type = "paged", 
                        page.details = TRUE,
                        page.revest = TRUE, 
                        page.budcomp= TRUE,
                        page.stim = TRUE,
                        page.sam = TRUE,
                        page.found = TRUE,
                        page.revexp = TRUE,
                        page.cac = TRUE,
                        page.gfb = TRUE,
                        page.transfers = TRUE,
                        page.methods = TRUE){
    
    tt_dir_set_projects()
    
    format <- {{ type }}
    
    FA_Data_District <-  
      df %>%
      filter(dnum == {{ dcode }},
             FY > (fiscal.year - 10)) %>%
      arrange(FY) %>% 
      ungroup()
    
    
    district_label <-  
      str_replace(FA_Data_District$district.label[1], " ", "-") %>% 
      str_replace("'", "")
    
    district_name <-  
      tools::toTitleCase(tolower(as.character( FA_Data_District$cy.d.8.dname[1])))
    
    district_code <- 
      FA_Data_District$dcode[1]
    
    
    rmarkdown::render(
      input = TannersTools::tt_dir_projects("financial.analysis", "R", "fa_output_dist_fa_template.Rmd"),
      output_file = paste0("FA_", fiscal.year, "_", dcode, "_", district_label, ".html"),
      output_dir = TannersTools::tt_dir_projects("financial.analysis", "output"),
      params = list(district =       {{ dcode }},
                    page_details=    {{ page.details }},
                    page_revest =    {{ page.revest }},
                    page_budcomp =   {{ page.budcomp }},
                    page_stim =      {{ page.stim }},
                    page_sam =       {{ page.sam }},
                    page_found =     {{ page.found }},
                    page_revexp =    {{ page.revexp }},
                    page_cac =       {{ page.cac }},
                    page_gfb =       {{ page.gfb }},
                    page_transfers = {{ page.transfers }},
                    page_methods =   {{ page.methods }}),
      
      
      
      if(format == "web"){
        
        output_format = rmarkdown::html_document(toc = TRUE, 
                                                 toc_float = TRUE, 
                                                 number_sections = FALSE,
                                                 css = "custom-page.css",
                                                 front_cover = "Front-Cover.svg")
        
        
      } else if(format == "paged"){
        
        output_format = pagedown::html_paged(toc = TRUE, 
                                             number_sections = FALSE,
                                             css = c("default-fonts", "custom-page.css", "default"),
                                             front_cover = "Front-Cover.svg"
        )
        
      } 
      
    ) 
    
    
    if(format == "paged"){
      setwd("FA/Output")
      pagedown::chrome_print(paste0("FA_", fiscal.year, "_", dcode, "_", district_label, ".html"),
                             output =  paste0("FA_", fiscal.year, "_", dcode, "_", district_label,".pdf"))
      
    }
    
  }



#' FA Render a bunch of financial analyses 
#'
#' 
#' 
#'
#' @export
fa_output_district_fa <- function(dnum_fa_requested) {
  
  start_time <- Sys.time()
  
  fa_output_possibly_render_district_fa <- possibly(fa_helper_render_district_fa, otherwise = "Error: Could Not Render")
  purrr::map({{ dnum_fa_requested }}, ~fa_output_possibly_render_district_fa(df = FA_Data, dcode = .))
  
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  
  
}


