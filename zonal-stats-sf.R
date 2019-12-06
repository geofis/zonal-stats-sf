zstatsf <- function(x = NULL, y = NULL, grpx = NULL, grpy = NULL) {
  #Example:
  # zstatsf(
  #   x = 'basins_parra_ocoa.gpkg',
  #   y = 'litologias.gpkg',
  #   grpx = 'value', grpy = 'DLO')
  # zstatsf(
  #   x = 'basins_order_4_ocoa.gpkg',
  #   y = 'litologias.gpkg',
  #   grpx = 'cat', grpy = 'DLO')
  suppressWarnings({
    require(sf)
    require(tidyverse)
    
    xsf <- st_read(x, quiet = T)
    ysf <- st_read(y, quiet = T)
    
    xsf <- xsf %>% rename(varx = contains(grpx))
    ysf <- ysf %>% rename(vary = matches(grpy))
    
    inters <- xsf %>%
      st_intersection(ysf %>% select(vary)) %>% 
      mutate(area=st_area(.)) %>% 
      group_by(varx) %>%
      mutate(pctarea=area/sum(area)*100) %>%
      dplyr::select(varx, vary, 'pctarea') %>%
      group_by(varx, vary) %>%
      summarise(pctarea=sum(pctarea)) %>%
      st_drop_geometry() %>%
      spread(vary, pctarea, fill = 0) %>%
      ungroup() %>% 
      inner_join(xsf %>% select(varx), by = 'varx') %>%
      rename(!!sym(grpx):=varx) %>% 
      st_as_sf
    return(inters)
  })
}
