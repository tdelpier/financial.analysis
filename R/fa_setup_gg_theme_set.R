



#' FA Set ggplot Theme
#'
#'
#' @export
fa_setup_gg_theme_set <- function() {
  
    theme_set(theme(
    plot.background = element_blank(),
    text = element_text(size= text_size,  family = "Helvetica"),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0, vjust = 1, face = "bold", size = text_size + 0.5),
    plot.subtitle = element_text(hjust = 0, vjust = 1, size = text_size),
    axis.text.x = element_text(angle = 0, hjust = .5, size = text_size),
    axis.text.y = element_text(angle = 0, hjust = 1, size = text_size),
    # strip.text = element_text(hjust = .5, size = 12, margin = margin(t =.4, unit = "cm")),
    axis.line = element_line(color= mea_gray, linewidth = .75),
    strip.background = element_blank(),
    plot.caption = element_text(hjust = 1, size = text_size),
    legend.position = "none"
  ))
  
  
}
