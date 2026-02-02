

#' FA Set Flextable Theme
#'
#'
#' @export
fa_setup_flextable_set <- function() {
  
  
  
  # https://ardata-fr.github.io/flextable-book/
  # https://davidgohel.github.io/flextable/index.html
  
  set_flextable_defaults(border.color = mea_gray)
 
  
  
}



#' FA flextable cutom theme
#'
#'
#' @export
flextable_custom_theme <- function(flex_table, title, line_spacing = 0, padding = 0){
  
  flextable_common_settings <- 
    {{ flex_table }} %>% 
    flextable::font(fontname = "Helvetica", part = "all") %>% 
    
    set_caption(caption = as_paragraph(as_chunk({{ title }}, propos = fp_text_default(font.family = "Helvetica"))) ,
                fp_p = officer::fp_par(text.align = "left"),
                align_with_table = FALSE) %>% 
    
    
    fontsize(size = 10, part = "all") %>% 
    
    align(align = "center", part = "header") %>% 
    bold(bold = TRUE, part = "header") 
  
  
  if({line_spacing} == 0 & {padding} == 0){
    
    output_flextable <- 
      flextable_common_settings %>% 
      line_spacing(space = .5, part = "body") %>%
      autofit()  %>%
      htmltools_value(ft.shadow = FALSE)
    
      
    
  } else{
    
    output_flextable <- 
      flextable_common_settings %>% 
      autofit()  %>%
      line_spacing(space = { line_spacing }, part = "body") %>%
      padding(padding.top = { padding }, padding.bottom = { padding }, part = "body") %>%
      htmltools_value(ft.shadow = FALSE)
    
    
  }
  
 return(output_flextable)
  
} 



# flextable_custom_theme <- function(x){
#   
#   #all
#   x <- fontsize(x, size = 10, part = "all")
#   
#   # header
#   x <- align(x, align = "left", part = "header")
#   x <- bold(x, bold = TRUE, part = "header")
#   x <- hline_top(x, border = fp_border_default(width = 0), part = "header")
#   x <- line_spacing(x, space = 0.5, part = "body")
#   
#   # body
#   
#   # x  <- font(x, fontname = "Arial", part = "all")
#   
#   x %>%
#     autofit() %>%   # prints x at the end
#     htmltools_value(ft.shadow = FALSE)
#   
# } 
