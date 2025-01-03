


#' FA District inlay map
#'
#' Creates a map of district as well as a map of the state 
#' to show where the district is. 
#'
#' @param df_sf supply districg geography as a sf object 
#' @param dnum dnum of district for map
#' @export
fa_output_map_dist_inlay <- function(df_sf, dnum){
  
  n <- 
    df_sf %>% 
    tibble() %>% 
    filter(dnum == {{ dnum }}) %>% 
    select(c_id) %>% 
    pull() 
  
  adjacent_districts <- st_intersects(df_sf, df_sf[n, 1], sparse = FALSE)
  district_adj_geo <- df_sf[adjacent_districts,] %>% 
    mutate(highlight = ifelse(dnum == {{ dnum }}, TRUE, FALSE))
  
  xmin <-  st_bbox(district_adj_geo)[[1]]
  ymin <-  st_bbox(district_adj_geo)[[2]]
  xmax <-  st_bbox(district_adj_geo)[[3]]
  ymax <-  st_bbox(district_adj_geo)[[4]]
  
  # big map
  state.map <- 
    ggplot() +
    geom_sf(data = df_sf, aes(geometry = geometry), size = 0, fill = "white")+
    geom_sf(data = district_adj_geo, aes(geometry = geometry, fill = highlight), size = 0)+
    geom_rect(aes(xmin = xmin, 
                  ymin = ymin,
                  xmax = xmax,
                  ymax = ymax),
              color = "black",
              fill = NA,
              size = .75)+
    scale_fill_manual(values = c("gray70", "#C10230")) +
    coord_sf(lims_method = "geometry_bbox") +
    coord_sf(ylim = c(41.75, 47.2), xlim = c(-82.4,-90.1), expand = TRUE) + 
    theme_void()+
    theme(legend.position = "none")
  
  # mini map
  mini.map <- 
    ggplot(data = district_adj_geo) +
    geom_sf(aes(geometry = geometry, fill = highlight), size = 0)+
    scale_fill_manual(values = c("gray70", "#C10230"))+
    geom_sf_text_repel(aes(label = district.label),
                       size = 2.75,
                       color = "black",
                       bg.color = "white",
                       bg.r = .3,
                       box.padding = 0.5) +
    geom_rect(aes(xmin = xmin, 
                  ymin = ymin,
                  xmax = xmax,
                  ymax = ymax),
              color = "black",
              fill = NA,
              size = .75)+
    coord_sf(lims_method = "geometry_bbox")+
    theme_void()+
    theme(legend.position = "none")
  
  
  map <- plot_grid(mini.map, state.map 
                   # scale = .8,
  )
  
  return(map)
  
}
