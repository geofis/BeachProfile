profiles <- function(transects = transprof, height = dsm, pointsPerPixel= 1, movingAvgK = 3) {
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
    l <- transects %>% filter(transect == x) %>% st_cast('LINESTRING', warn = F)
    p <- st_line_sample(l, density = pointsPerPixel/res(height)[1]) %>% st_cast('POINT', warn = F)
    ref <- p[1]
    d <- st_distance(p, ref) %>% as.vector()
    v <- raster::extract(height, as_Spatial(p))
    df <- data.frame(dist = d, h = v)
    return(df)
  }) %>% setNames(index)
  dfdmn <- plyr::ldply(ldmn, .id = 'transect') %>%
    na.omit %>% 
    mutate_if(is.factor, as.character) %>% 
    group_by(transect) %>% 
    mutate(hma = rollmean(x = h, k =  movingAvgK, na.pad=TRUE)) %>% 
    mutate(distlm = predict(lm(dist ~ hma, na.action = na.exclude)) + residuals(lm(dist ~ hma, na.action = na.exclude))) %>%
    do(add_row(
      .,
      transect = unique(.$transect),
      hma=0,
      distlm = predict(lm(dist ~ hma, .), data.frame(hma=0)))) %>%
    ungroup %>% 
    mutate(transect = as.factor(transect))
  dfdmnls <- dfdmn %>% 
    group_by(transect) %>% 
    dplyr::select(transect, distlm, hma) %>% 
    mutate(
      distlm = rescale(distlm, to = c(0, 1)),
      hma = rescale(hma, to = c(0, 1))
      ) %>% 
    ungroup
  dfdmnlsrawd <- dfdmn %>% 
    group_by(transect) %>% 
    dplyr::select(transect, dist, hma) %>% 
    mutate(
      dist = rescale(dist, to = c(0, 1)),
      hma = rescale(hma, to = c(0, 1))
    ) %>% 
    ungroup
  dfci <- dfdmnls %>%
    group_by(transect) %>%
    dplyr::select(transect, distlm, hma) %>% 
    na.omit %>% 
    summarise(ci = (0.5 - AUC(distlm, hma)) / 0.5)
  dfcirawd <- dfdmnlsrawd %>%
    group_by(transect) %>%
    dplyr::select(transect, dist, hma) %>% 
    na.omit %>% 
    summarise(ci = (0.5 - AUC(dist, hma)) / 0.5)
  dfdmn$dist <- set_units(dfdmn$dist, 'meters')
  dfdmn$distlm <- set_units(dfdmn$distlm, 'meters')
  dfdmn$h <- set_units(dfdmn$h, 'meters')
  dfdmn$hma <- set_units(dfdmn$hma, 'meters')
  dfsl <- dfdmn %>%
    group_by(transect) %>%
    dplyr::select(transect, dist, distlm, hma) %>% 
    summarise(
      dh = max(hma, na.rm = T) - min(hma, na.rm = T),
      ddistlm = max(distlm, na.rm = T) - min(distlm, na.rm = T),
      slope = set_units(coef(lm(hma~distlm))[[2]], '1'),
      slopeRad = atan(slope),
      slopeDeg = set_units(slopeRad, 'degrees'),
      distrawd = max(dist, na.rm = T) - min(dist, na.rm = T),
      sloperawd = set_units(coef(lm(hma~dist))[[2]], '1'),
      sloperawdRad = atan(sloperawd),
      sloperawdDeg = set_units(sloperawdRad, 'degrees')
    )
  return(
    list(
      dimension = dfdmn, 
      dimensionless = dfdmnls, 
      dimensionlessrawdistance = dfdmnlsrawd, 
      concavityindex = dfci, 
      concavityindexrawdistance = dfcirawd, 
      slope = dfsl))
}
