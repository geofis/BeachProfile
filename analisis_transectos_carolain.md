BeachProfile, Carolaine’s transects
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

BeachProfile is a set of R tools that enable users to extract beach
profiles from a digital surface model. The tools generate elevation
profile graphs showing the slope, a table of associated data, as well as
profile concavity calculations. Hopefully, these tools, along with those
of the [RCoastSat repo](https://github.com/geofis/RCoastSat) for
analysis of time-series of shoreline extracted with
[CoastSat](https://github.com/kvos/CoastSat) (an open-source software
toolkit written in Python by @kvos), will be part of an R package.

Here I show how to generate 20 topographic profiles of Najayo Beach (Los
Pescadores sector), located in the south-central coast of the Dominican
Republic. The input data include a 10-cm resolution digital surface
model (DSM) generated from an aerial survey processed with OpenDroneMap
and GRASS GIS, as well as a set of transects previously digitized in
QGIS.

![Orthophoto and transects (yellow) as shown in
QGIS](img/transects-qgis-carolaine.jpg)

This repo, the field work and the data collected, are part of assignment
projects for the Geomorphology course (Geography Degree), Autonomous
University of Santo Domingo. More details in this
[video](https://www.youtube.com/watch?v=k6j5pVxvfN0) (in Spanish).

## Packages

``` r
library(tidyverse)
library(purrr)
library(sf)
library(RColorBrewer)
library(raster)
```

## Read the functions

``` r
funs <- list.files('R', pattern = '*.R', full.names = T)
map(funs, source)
```

## Import/plot transects and shorelines

``` r
transprof <- rtrans('data/perfil_los_pescadores.geojson')#Digitized by Carolain, edited by geofis
## Reading layer `transectos_ortofotos' from data source `/home/jr/Documentos/git/BeachProfile/data/perfil_los_pescadores.geojson' using driver `GeoJSON'
## Simple feature collection with 20 features and 1 field
## geometry type:  LINESTRING
## dimension:      XY
## bbox:           xmin: 383443.4 ymin: 2024230 xmax: 383656.2 ymax: 2024320
## epsg (SRID):    32619
## proj4string:    +proj=utm +zone=19 +datum=WGS84 +units=m +no_defs
rawDsm <- raster('data/raw-dsm-pescadores.tif')
dsm <- thresholdRaise(rasterDsm = rawDsm, threshold = -44.1)
plot(dsm)
plot(as_Spatial(transprof), add=T)
```

<img src="img/carolain-transect-1.png" width="100%" />

``` r

#ggplot of transects
cols <- colorRampPalette(brewer.pal(9,'Set1'))(nrow(transprof))
ggplot() +
  geom_sf(data = transprof, color = cols) +
  scale_color_manual(values = c('black', 'orange', 'blue')) +
  geom_sf_text(
    data = transprof %>%
      st_centroid, aes(label = transect), size = 3) +
  theme_minimal() +
  theme(legend.title = element_blank())
## Warning in st_centroid.sf(.): st_centroid assumes attributes are constant
## over geometries of x
```

<img src="img/carolain-transect-2.png" width="100%" />

## Profile data

``` r
profData_temp <- profiles(transects = transprof, height = dsm,
                          pointsPerPixel= 2, movingAvgK = 3)
## rgeos version: 0.5-1, (SVN revision 614)
##  GEOS runtime version: 3.6.2-CAPI-1.10.2 
##  Linking to sp version: 1.3-1 
##  Polygon checking: TRUE
## 
## Attaching package: 'scales'
## The following object is masked from 'package:purrr':
## 
##     discard
## The following object is masked from 'package:readr':
## 
##     col_factor
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
## udunits system database from /usr/share/xml/udunits
profData_temp
## $dimension
## # A tibble: 2,603 x 5
##    transect       dist        h      hma     distlm
##    <fct>           [m]      [m]      [m]        [m]
##  1 1        0.00000000 2.447498       NA         NA
##  2 1        0.04982810 2.447498 2.447221 0.04982810
##  3 1        0.09965621 2.446667 2.446944 0.09965621
##  4 1        0.14948432 2.446667 2.436445 0.14948432
##  5 1        0.19931242 2.416000 2.426222 0.19931242
##  6 1        0.24914053 2.416000 2.402000 0.24914053
##  7 1        0.29896863 2.374001 2.388000 0.29896863
##  8 1        0.34879674 2.374001 2.352666 0.34879674
##  9 1        0.39862484 2.309998 2.331332 0.39862484
## 10 1        0.44845295 2.309998 2.291665 0.44845295
## # … with 2,593 more rows
## 
## $dimensionless
## # A tibble: 2,603 x 3
##    transect   distlm    hma
##    <fct>       <dbl>  <dbl>
##  1 1        NA       NA    
##  2 1         0        1    
##  3 1         0.00678  1.000
##  4 1         0.0136   0.996
##  5 1         0.0203   0.991
##  6 1         0.0271   0.982
##  7 1         0.0339   0.976
##  8 1         0.0407   0.961
##  9 1         0.0475   0.953
## 10 1         0.0542   0.936
## # … with 2,593 more rows
## 
## $dimensionlessrawdistance
## # A tibble: 2,603 x 3
##    transect   dist    hma
##    <fct>     <dbl>  <dbl>
##  1 1        0      NA    
##  2 1        0.0143  1    
##  3 1        0.0286  1.000
##  4 1        0.0429  0.996
##  5 1        0.0571  0.991
##  6 1        0.0714  0.982
##  7 1        0.0857  0.976
##  8 1        0.1000  0.961
##  9 1        0.114   0.953
## 10 1        0.129   0.936
## # … with 2,593 more rows
## 
## $concavityindex
## # A tibble: 20 x 2
##    transect       ci
##    <fct>       <dbl>
##  1 1         0.0136 
##  2 10        0.188  
##  3 11        0.371  
##  4 12        0.281  
##  5 13        0.149  
##  6 14        0.0692 
##  7 15        0.127  
##  8 16        0.0530 
##  9 17       -0.0744 
## 10 18       -0.0256 
## 11 19       -0.0809 
## 12 2         0.0173 
## 13 20        0.0441 
## 14 3         0.00273
## 15 4        -0.0295 
## 16 5        -0.0686 
## 17 6        -0.0428 
## 18 7         0.0276 
## 19 8         0.0960 
## 20 9         0.0880 
## 
## $concavityindexrawdistance
## # A tibble: 20 x 2
##    transect      ci
##    <fct>      <dbl>
##  1 1        -0.454 
##  2 10        0.196 
##  3 11        0.381 
##  4 12        0.290 
##  5 13        0.0927
##  6 14       -0.0533
##  7 15       -0.267 
##  8 16       -0.335 
##  9 17       -0.364 
## 10 18       -0.605 
## 11 19       -0.693 
## 12 2        -0.261 
## 13 20       -0.699 
## 14 3        -0.412 
## 15 4        -0.420 
## 16 5        -0.228 
## 17 6        -0.218 
## 18 7        -0.141 
## 19 8        -0.0366
## 20 9        -0.0357
## 
## $slope
## # A tibble: 20 x 10
##    transect       dh  ddistlm    slope slopeRad slopeDeg distrawd sloperawd
##    <fct>         [m]      [m]      [1]    [rad]      [°]      [m]       [1]
##  1 1        2.44722…  7.3495… -0.3077… -0.2985… -17.105… 3.487967 -0.29995…
##  2 10       0.69277…  7.1472… -0.0495… -0.0495…  -2.838… 7.220751 -0.04896…
##  3 11       0.79599…  5.5760… -0.1243… -0.1237…  -7.088… 5.675620 -0.12404…
##  4 12       0.50111…  6.1443… -0.0582… -0.0582…  -3.335… 6.244295 -0.05801…
##  5 13       1.18099…  8.9929… -0.0992… -0.0988…  -5.665… 8.384193 -0.09890…
##  6 14       1.47791… 11.4141… -0.1163… -0.1158…  -6.637… 9.903715 -0.11625…
##  7 15       1.63559… 12.7956… -0.1067… -0.1063…  -6.095… 7.740399 -0.10570…
##  8 16       2.06222… 13.9498… -0.1180… -0.1175…  -6.732… 8.195824 -0.11558…
##  9 17       2.19796…  9.3141… -0.2411… -0.2366… -13.556… 6.739649 -0.24007…
## 10 18       3.23785… 14.1406… -0.2272… -0.2234… -12.800… 6.566238 -0.22167…
## 11 19       2.58242… 16.7464… -0.1330… -0.1322…  -7.580… 8.001098 -0.12513…
## 12 2        1.98118…  5.6527… -0.3450… -0.3322… -19.034… 4.021715 -0.34471…
## 13 20       3.93166… 19.0466… -0.2038… -0.2010… -11.519… 6.080869 -0.19600…
## 14 3        1.71110…  6.4661… -0.2595… -0.2539… -14.552… 3.585818 -0.25854…
## 15 4        1.48999…  7.9341… -0.1942… -0.1918… -10.991… 5.237396 -0.19329…
## 16 5        1.24952…  7.6193… -0.1697… -0.1680…  -9.631… 6.446877 -0.16943…
## 17 6        1.03875…  7.3895… -0.1299… -0.1292…  -7.406… 6.261463 -0.12911…
## 18 7        0.93711…  7.8793… -0.1004… -0.1001…  -5.738… 6.625684 -0.09973…
## 19 8        0.78908…  7.7018… -0.0528… -0.0528…  -3.025… 6.356535 -0.05095…
## 20 9        0.76317…  8.1651… -0.0673… -0.0672…  -3.852… 7.082702 -0.06664…
## # … with 2 more variables: sloperawdRad [rad], sloperawdDeg [°]
```

## Prepare data to accommodate 48 profiles

``` r
profData <- lapply(profData_temp, function(x)
  x %>% mutate(
    transect2 = as.numeric(as.character(transect)),
    group = cut(transect2,
                breaks = c(0, 5, 10, 15, 20),
                labels = c('1-5', '6-10', '11-15', '16-20')),
  transect = forcats::fct_relevel(
    transect, function(x){as.character(sort(as.integer(x)))})
  )
)
```

## Profile plots

### Dimensionsional profiles

#### Profiles match their actual digitized extension (raw distance)

##### xy scales different, scale non-consistent across panels

``` r
#Raw distance
dmngridrawd <- sapply(as.character(unique(profData$dimension$group)), function(x) {
  profData$dimension %>% filter(group==x) %>% drop_units %>% droplevels %>%
    ggplot() +
      aes(x = dist, y = hma) +
      geom_path(col = 'red', lwd = 1, na.rm = T) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_y_continuous(breaks = pretty_breaks()) +
      expand_limits(y = -0.05) +
      ylab('Height, moving average (m)') + xlab('Raw distance, landward origin (m)') +
      geom_text(
        data = profData$slope %>% filter(group==x) %>% drop_units %>% droplevels,
        aes(x = 0, y = 0, label = paste0('m=', round(sloperawdDeg,2), '°')),
        size = 3,
        hjust = 0,
        parse = F
      ) +
      facet_wrap(~transect, nrow = 2, scales = 'free') +
      theme_bw() + 
      theme(text = element_text(size = 8))
}, simplify = F, USE.NAMES = T)
invisible(sapply(
  names(dmngridrawd),
  function(x) print(dmngridrawd[[x]])
))
```

<img src="img/carolain-raw-distance-diff-scales-xy-and-panels-1.png" width="100%" /><img src="img/carolain-raw-distance-diff-scales-xy-and-panels-2.png" width="100%" /><img src="img/carolain-raw-distance-diff-scales-xy-and-panels-3.png" width="100%" /><img src="img/carolain-raw-distance-diff-scales-xy-and-panels-4.png" width="100%" />

##### xy scales different, scale consistent across panels of the same plot

``` r
invisible(sapply(
  names(dmngridrawd),
  function(x) {
    p <- dmngridrawd[[x]] +
      stat_smooth(
        aes(x = dist, y = hma), geom = 'line', color = 'black',
        alpha = 0.5, formula = y~x, method = 'lm', na.rm = T) +
      facet_wrap(~transect, nrow = 2)
    print(p)
  }
))
```

<img src="img/carolain-raw-distance-diff-scales-xy-same-across-panels-1.png" width="100%" /><img src="img/carolain-raw-distance-diff-scales-xy-same-across-panels-2.png" width="100%" /><img src="img/carolain-raw-distance-diff-scales-xy-same-across-panels-3.png" width="100%" /><img src="img/carolain-raw-distance-diff-scales-xy-same-across-panels-4.png" width="100%" />

##### xy scales equal (no vertical exaggeration), scale consistent across panels of the same plot

``` r
#Equal xy scales, no vertical exaggeration
invisible(sapply(
  names(dmngridrawd),
  function(x) {
    p <- dmngridrawd[[x]] + facet_wrap(~transect, nrow = 5) + coord_equal() #Not so informative but panels are comparable between each other
    print(p)
  }
))
```

<img src="img/carolain-raw-distance-same-scale-xy-same-across-panels-1.png" width="100%" /><img src="img/carolain-raw-distance-same-scale-xy-same-across-panels-2.png" width="100%" /><img src="img/carolain-raw-distance-same-scale-xy-same-across-panels-3.png" width="100%" /><img src="img/carolain-raw-distance-same-scale-xy-same-across-panels-4.png" width="100%" />

##### Vertical exaggeration 2x, scale consistent across panels of the same plot

``` r
invisible(sapply(
  names(dmngridrawd),
  function(x) {
    p <- dmngridrawd[[x]] + facet_wrap(~transect, nrow = 5) + coord_equal(ratio = 2) #Not so informative but panels are comparable
    print(p)
  }
))
```

<img src="img/carolain-raw-distance-vert-exag-2x-same-across-panels-1.png" width="100%" /><img src="img/carolain-raw-distance-vert-exag-2x-same-across-panels-2.png" width="100%" /><img src="img/carolain-raw-distance-vert-exag-2x-same-across-panels-3.png" width="100%" /><img src="img/carolain-raw-distance-vert-exag-2x-same-across-panels-4.png" width="100%" />

#### Profiles extended to a conventionally chosen zero using linear regression

##### xy scales different, scale non-consistent across panels

``` r
dmngrid <- sapply(as.character(unique(profData$dimension$group)), function(x) {
  profData$dimension %>% filter(group==x) %>% drop_units %>% ggplot() +
    aes(x = distlm, y = hma) +
    geom_line(col = 'red', lwd = 1, na.rm = T) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks()) +
    expand_limits(y = -0.05) +
    ylab('Height, moving average (m)') + xlab('Distance, landward origin (m)') +
    geom_text(
      data = profData$slope %>% filter(group==x) %>% drop_units,
      aes(x = 0, y = 0, label = paste0('m=', round(slopeDeg,2), '°')),
      size = 3,
      hjust = 0,
      parse = F
    ) +
    facet_wrap(~transect, nrow = 2, scales = 'free') +
    theme_bw() + 
    theme(text = element_text(size = 8))
}, simplify = F, USE.NAMES = T)

invisible(sapply(
  names(dmngrid),
  function(x) {
    print(dmngrid[[x]])
  }
))
```

<img src="img/carolain-extended-distance-diff-scales-xy-and-panels-1.png" width="100%" /><img src="img/carolain-extended-distance-diff-scales-xy-and-panels-2.png" width="100%" /><img src="img/carolain-extended-distance-diff-scales-xy-and-panels-3.png" width="100%" /><img src="img/carolain-extended-distance-diff-scales-xy-and-panels-4.png" width="100%" />

##### xy scales different, scale consistent across panels of the same plot

``` r
invisible(sapply(
  names(dmngrid),
  function(x) {
    p <- dmngrid[[x]] +
      stat_smooth(
        aes(x = distlm, y = hma), geom = 'line', color = 'black',
        alpha = 0.5, formula = y~x, method = 'lm', na.rm = T) +
      facet_wrap(~transect, nrow = 2) 
    print(p)
  }
))
```

<img src="img/carolain-extended-distance-diff-scales-xy-same-across-panels-1.png" width="100%" /><img src="img/carolain-extended-distance-diff-scales-xy-same-across-panels-2.png" width="100%" /><img src="img/carolain-extended-distance-diff-scales-xy-same-across-panels-3.png" width="100%" /><img src="img/carolain-extended-distance-diff-scales-xy-same-across-panels-4.png" width="100%" />

##### xy scales equal (no vertical exaggeration), scale consistent across panels of the same plot

``` r
invisible(sapply(
  names(dmngrid),
  function(x) {
    p <- dmngrid[[x]] +
      stat_smooth(
        aes(x = distlm, y = hma), geom = 'line', color = 'black',
        alpha = 0.5, formula = y~x, method = 'lm', na.rm = T) +
      facet_wrap(~transect, nrow = 5) + coord_equal()
    print(p)
  }
))
```

<img src="img/carolain-extended-distance-same-scale-xy-same-across-panels-1.png" width="100%" /><img src="img/carolain-extended-distance-same-scale-xy-same-across-panels-2.png" width="100%" /><img src="img/carolain-extended-distance-same-scale-xy-same-across-panels-3.png" width="100%" /><img src="img/carolain-extended-distance-same-scale-xy-same-across-panels-4.png" width="100%" />
\#\#\#\#\# Vertical exaggeration 2x, scale consistent across panels of
the same plot

``` r
invisible(sapply(
  names(dmngrid),
  function(x) {
    p <- dmngrid[[x]] + facet_wrap(~transect, nrow = 5) + coord_equal(ratio = 2) #Not so informative but panels are comparable
    print(p)
  }
))
```

<img src="img/carolain-extended-distance-vert-exag-2x-same-across-panels-1.png" width="100%" /><img src="img/carolain-extended-distance-vert-exag-2x-same-across-panels-2.png" width="100%" /><img src="img/carolain-extended-distance-vert-exag-2x-same-across-panels-3.png" width="100%" /><img src="img/carolain-extended-distance-vert-exag-2x-same-across-panels-4.png" width="100%" />

### Dimensionless, profile concavity indices

#### Profiles match their actual digitized extension

``` r
dmnlsgridrawd1 <- sapply(
  as.character(unique(profData$dimensionlessrawdistance$group)), function(x) {
    profData$dimensionlessrawdistance %>% filter(group == x) %>% na.omit %>% ggplot() +
      aes(x = dist, y = hma) +
      geom_line(col = 'red', lwd = 1) +
      scale_x_continuous(breaks = pretty_breaks(), limits = c(0,1)) +
      scale_y_continuous(breaks = pretty_breaks(), limits = c(0,1)) +
      geom_text(
        data = profData$concavityindexrawdistance %>% filter(group==x),
          aes(x = 0.1, y = 0.1, label = paste0('C[a]==', round(ci,2))),
          size = 3,
          hjust = 0,
          parse = T
      ) +
      coord_equal() +
      facet_wrap(~transect, nrow = 3) +
      theme_bw() + 
      theme(text = element_text(size = 9))
    }, simplify = F, USE.NAMES = T
)
invisible(sapply(
  names(dmnlsgridrawd1),
  function(x) print(dmnlsgridrawd1[[x]])
))
```

<img src="img/carolain-dimensionless-raw-distance-profile-concavity-1.png" width="100%" /><img src="img/carolain-dimensionless-raw-distance-profile-concavity-2.png" width="100%" /><img src="img/carolain-dimensionless-raw-distance-profile-concavity-3.png" width="100%" /><img src="img/carolain-dimensionless-raw-distance-profile-concavity-4.png" width="100%" />

#### Profiles extended to a conventionally chosen zero using linear regression

``` r
dmnlsgrid1 <- sapply(
  as.character(unique(profData$dimensionless$group)), function(x) {
    profData$dimensionless %>% filter(group == x) %>% na.omit %>% ggplot() +
      aes(x = distlm, y = hma) +
      geom_line(col = 'red', lwd = 1) +
      scale_x_continuous(breaks = pretty_breaks(), limits = c(0,1)) +
      scale_y_continuous(breaks = pretty_breaks(), limits = c(0,1)) +
      geom_text(
        data = profData$concavityindex %>% filter(group==x),
          aes(x = 0.1, y = 0.1, label = paste0('C[a]==', round(ci,2))),
          size = 3,
          hjust = 0,
          parse = T
      ) +
      coord_equal() +
      facet_wrap(~transect, nrow = 3) +
      theme_bw() + 
      theme(text = element_text(size = 9))
    }, simplify = F, USE.NAMES = T
)
invisible(sapply(
  names(dmnlsgrid1),
  function(x) print(dmnlsgrid1[[x]])
))
```

<img src="img/carolain-dimensionless-extended-distance-profile-concavity-1.png" width="100%" /><img src="img/carolain-dimensionless-extended-distance-profile-concavity-2.png" width="100%" /><img src="img/carolain-dimensionless-extended-distance-profile-concavity-3.png" width="100%" /><img src="img/carolain-dimensionless-extended-distance-profile-concavity-4.png" width="100%" />
