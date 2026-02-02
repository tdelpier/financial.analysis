



# TannersTools::tt_dir_projects("financial.analysis", "R", "fa_output_dist_fa_template.Rmd")
  

#' FA Render one district financial analysis
#'
#' 
#'
#' @export
fa_helper_render_district_fa <- function(df, dcode, type = "paged", 
                        page.details = TRUE,
                        page.revest = TRUE, 
                        page.budcomp= TRUE,
                        page.sam = TRUE,
                        page.found = TRUE,
                        page.revexp = TRUE,
                        page.surplus = FALSE,
                        page.cac = FALSE,
                        page.gfb = TRUE,
                        page.transfers = TRUE,
                        page.transfers.alt,
                        page.millages = TRUE,
                        page.stutransfers = TRUE,
                        page.econdisad = TRUE, 
                        page.methods = TRUE,
                        page.stim = FALSE
                        ){
    
  
  quietly(tt_dir_projects())
    
    
    format <- {{ type }}
    
    FA_Data_District <<-  
      df %>%
      filter(dnum == {{ dcode }},
             FY >= (fiscal.year - 10)) %>%
      arrange(FY) %>% 
      ungroup()
    
    
    district_label <-  
      str_replace(FA_Data_District$district.label[1], " ", "-") %>% 
      str_replace("'", "")
    
    district_name <-  
      tools::toTitleCase(tolower(as.character( FA_Data_District$cy.d.8.dname[1])))
    
    district_code <- 
      FA_Data_District$dcode[1]
    
    
    fa_helper_do_transfers_page(FA_Data_District, {{ page.transfers }})
    
    
    rmarkdown::render(
      input = TannersTools::tt_dir_projects("financial.analysis", "R", "fa_output_dist_fa_template.Rmd"),
      output_file = paste0("FA_", fiscal.year, "_", dcode, "_", district_label, ".html"),
      output_dir = TannersTools::tt_dir_projects("financial.analysis", "output"),
      quiet = TRUE,
      params = list(district =       {{ dcode }},
                    page_details=    {{ page.details }},
                    page_revest =    {{ page.revest }},
                    page_budcomp =   {{ page.budcomp }},
                    page_stim =      {{ page.stim }},
                    page_sam =       {{ page.sam }},
                    page_found =     {{ page.found }},
                    page_revexp =    {{ page.revexp }},
                    page_surplus =   {{ page.surplus }},
                    page_cac =       {{ page.cac }},
                    page_gfb =       {{ page.gfb }},
                    page_transfers = do_transfers_page,
                    page_transfers_alt = do_transfers_alt_page,
                    page_millages =  {{ page.millages }},
                    page_stu_transfers = {{ page.stutransfers }},
                    page_econ_disad = {{ page.econdisad }}, 
                    page_methods =   {{ page.methods }}),
      
      
      
      if(format == "web"){
        
        output_format = rmarkdown::html_document(toc = TRUE, 
                                                 toc_float = TRUE, 
                                                 number_sections = FALSE,
                                                 css = "custom-page.css",
                                                 front_cover = "Front-Cover.svg")
        
        
      } else if(format == "paged"){
        
        output_format = pagedown::html_paged(toc = TRUE, 
                                             # toc_depth = 1,
                                             number_sections = FALSE,
                                             css = c("default-fonts", "custom-page.css", "default"),
                                             front_cover = "Front-Cover.svg"
        )
        
      } else if(format == "slides"){
        
        output_format = rmarkdown::slidy_presentation()
        # output_format = rmarkdown::ioslides_presentation()

      }
      
    ) 
    
    
    if(format == "paged"){
      # setwd("FA/Output")
      
      TannersTools::tt_dir_projects("financial.analysis","output")
      pagedown::chrome_print(TannersTools::tt_dir_projects("financial.analysis","output", paste0("FA_", fiscal.year, "_", dcode, "_", district_label, ".html")),
                             output =  TannersTools::tt_dir_projects("financial.analysis","output", paste0("FA_", fiscal.year, "_", dcode, "_", district_label,".pdf")))
      
    }
    
    ### trying

    message("Completed: ", district_name,", ", district_code)
    
    
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
  elapsed_time <<- end_time - start_time
  
  message("Completed fa_output_district_fa function ✅" )  
  
}


