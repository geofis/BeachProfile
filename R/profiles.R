profiles <- function(transects = transprof, height = dsm, facetRows = 2, movingAvgK = 7) {
  library(rgeos)
  library(raster)
  library(scales)
  library(DescTools)
  library(tidyverse)
  library(sf)
  library(ggplot2)
  library(zoo)
  index <- transects %>% pull(transect)
  ldmnls <- map(index, function(x) {
    l <- transects %>% filter(grepl(x, transect)) %>% st_cast('LINESTRING', warn = F)
    p <- st_line_sample(l, density = 2/res(height)[1]) %>% st_cast('POINT', warn = F)
    ref <- p[1]
    d <- st_distance(p, ref) %>% units::drop_units() %>% as.vector()
    v <- raster::extract(height, as_Spatial(p))
    df <- data.frame(dist=rescale(d, to = c(0, 1)), h=rescale(v, to = c(0, 1)))
    return(df)
  }) %>% setNames(index)
  dfdmnls <- plyr::ldply(ldmnls, .id = 'transect')
  dfdmnls <- dfdmnls %>% group_by(transect) %>% mutate(hma=rollmean(x = h, k =  movingAvgK, na.pad=TRUE))
  dfci <- dfdmnls %>% group_by(transect) %>% summarise(ci = (0.5 - AUC(dist, h)) / 0.5)
  pl <- suppressWarnings(dfdmnls %>% ggplot() +
    aes(x = dist, y = hma) +
    geom_line(col = 'red', lwd = 1) +
    geom_text(
      data = dfci, aes(x = 0.1, y = 0.1, label = paste0('C[a]==', round(ci,2))),
      x = 0.1, y = 0.1,
      size = 4,
      hjust = 0,
      parse = T
    ) +
    coord_equal() +
    facet_wrap(~transect, nrow = facetRows) +
    theme_bw() + 
    theme(text = element_text(size = 14)))
  return(list(dfdmnls, pl))
}
