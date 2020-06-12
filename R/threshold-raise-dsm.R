thresholdRaise <- function(rasterDsm = NULL, threshold = NULL, outRasterPath = NULL, ...) {
  ifelse(n>=3, 'n should be an odd number 3 or greater')
  if (n < 3) stop('n should be an odd number 3 or greater')
  n <- ifelse(n%%2 == 0, n+1, n)
  rasterDsm[rasterDsm < threshold] <- NA
  raisedRaster <- rasterDsm + abs(min(rasterDsm[], na.rm = T))
  if(!is.null(outRasterPath)) writeRaster(raisedRaster, outRasterPath, ...)
  return(raisedRaster)
}
