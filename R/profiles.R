profiles <- function(transects = transprof, height = dsm, pointsPerPixel= 2, facetRows = 2, movingAvgK = 7) {
  library(rgeos)
  library(raster)
  library(scales)
  library(DescTools)
  library(tidyverse)
  library(sf)
  library(ggplot2)
  library(zoo)
  library(units)
  index <- transects %>% pull(transect)
  ldmn <- map(index, function(x) {
    l <- transects %>% filter(grepl(x, transect)) %>% st_cast('LINESTRING', warn = F)
    p <- st_line_sample(l, density = pointsPerPixel/res(height)[1]) %>% st_cast('POINT', warn = F)
    ref <- p[1]
    d <- st_distance(p, ref) %>% as.vector() #%>% drop_units()
    v <- raster::extract(height, as_Spatial(p))
    df <- data.frame(dist = d, h = v)
    return(df)
  }) %>% setNames(index)
  dfdmn <- plyr::ldply(ldmn, .id = 'transect') %>%
    group_by(transect) %>% 
    mutate(hma=rollmean(x = h, k =  movingAvgK, na.pad=TRUE)) %>% 
    ungroup
  dfdmnls <- dfdmn %>% 
    group_by(transect) %>% 
    mutate(
      dist = rescale(dist, to = c(0, 1)),
      h = rescale(h, to = c(0, 1)),
      hma = rescale(hma, to = c(0, 1))
      ) %>% 
    ungroup
  dfci <- dfdmnls %>% group_by(transect) %>% summarise(ci = (0.5 - AUC(dist, h)) / 0.5)
  dfdmn$dist <- set_units(dfdmn$dist, 'meters')
  dfdmn$h <- set_units(dfdmn$h, 'meters')
  dfdmn$hma <- set_units(dfdmn$hma, 'meters')
  dfsl <- dfdmn %>%
    group_by(transect) %>%
    summarise(
      dh = last(na.omit(h))-first(na.omit(h)),
      ddist = last(na.omit(dist))-first(na.omit(dist)),
      slope = dh/ddist, 
      slopeRad = atan(slope),
      slopeDeg = set_units(slopeRad, 'degrees')
      )
  return(list(dimension = dfdmn, dimensionless = dfdmnls, concavityindex = dfci, slope = dfsl))
  pl <- suppressWarnings(dfdmnls %>% ggplot() +
    aes(x = dist, y = hma) +
    geom_line(col = 'red', lwd = 1) +
    geom_text(
      data = dfci, aes(x = 0.1, y = 0.1, label = paste0('C[a]==', round(ci,2))),
      x = 0.1, y = 0.2,
      size = 3,
      hjust = 0,
      parse = T
    ) +
    coord_equal() +
    facet_wrap(~transect, nrow = facetRows) +
    theme_bw() + 
    theme(text = element_text(size = 12)))
  return(list(dfdmnls, pl))
}
