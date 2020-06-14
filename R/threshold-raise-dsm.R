thresholdRaise <- function(rasterDsm = NULL, threshold = NULL, outRasterPath = NULL, ...) {
  rasterDsm[rasterDsm < threshold] <- NA
  raisedRaster <- rasterDsm + abs(min(rasterDsm[], na.rm = T))
  if(!is.null(outRasterPath)) writeRaster(raisedRaster, outRasterPath, ...)
  return(raisedRaster)
}
