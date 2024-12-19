

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
flextable_custom_theme <- function(x){
  
  #all
  x <- fontsize(x, size = 10, part = "all")
  
  # header
  x <- align(x, align = "center", part = "header")
  x <- bold(x, bold = TRUE, part = "header")
  
  
  
  # body
  
  # x  <- font(x, fontname = "Arial", part = "all")
  
  x %>% 
    autofit() %>%   # prints x at the end
    htmltools_value(ft.shadow = FALSE)
  
} 

