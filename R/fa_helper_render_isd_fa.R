


#' FA Render one ISD financial analysis
#'
#' 
#'
#' @export
fa_helper_render_isd_fa<- function(dnum) {
  
  
  FA_Data_ISD <- 
    isd_data %>% 
    ungroup() %>% 
    filter(FY > (fiscal.year - 11),
           dnum == {{ dnum }})
  
  
  Transfer_Data_ISD <- 
    isd_transfers %>% 
    ungroup() %>% 
    filter(dnum == {{ dnum }})
  
  
  district_name <- FA_Data_ISD$dname[1]

  district_code <-
    FA_Data_ISD$dnum[1]
  
  
  rmarkdown::render(
    input = TannersTools::tt_dir_projects("financial.analysis", "R", "fa_output_isd_fa_template.Rmd"),
    output_dir = TannersTools::tt_dir_projects("financial.analysis", "output"),
    output_file = paste0("FA_", fiscal.year, "_", district_code, "_", district_name, ".html"),
    output_format = pagedown::html_paged(toc = TRUE, 
                                         # toc_depth = 1,
                                         number_sections = FALSE,
                                         css = c("default-fonts", "custom-page.css", "default"),
                                         front_cover = "Front-Cover.svg"))
  
  TannersTools::tt_dir_projects("financial.analysis","output")
  pagedown::chrome_print(TannersTools::tt_dir_projects("financial.analysis","output", paste0("FA_", fiscal.year, "_", district_code, "_", district_name, ".html")),
                         output =  TannersTools::tt_dir_projects("financial.analysis","output", paste0("FA_", fiscal.year, "_", district_code, "_", district_name, ".pdf")))
  
  
  
}




#' FA Render a bunch of ISD financial analyses 
#'
#' 
#' 
#'
#' @export
fa_output_isd_fa <- function(dnum_fa_requested) {
  
  start_time <- Sys.time()
  
  fa_output_possibly_render_isd_fa <- possibly(fa_helper_render_isd_fa, otherwise = "Error: Could Not Render")
  purrr::map({{ dnum_fa_requested }}, ~fa_output_possibly_render_isd_fa(dnum = .))
  
  end_time <- Sys.time()
  elapsed_time <<- end_time - start_time
  
  message("Completed fa_output_district_fa function ✅" )  
  
}
