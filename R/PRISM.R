#' @title Functions for PRISM manipulation
#'
#' @description  NA
#'
#' @name PRISM
#' @author Brandon McNellis
NULL
#' @rdname internals
#' @export
extract_PRISM <- function(fl, coords, by) {
  stopifnot(
    require(raster),
    require(sp),
    file.exists(fl)
  )

  rast0 <- raster(fl)
  rast_vec <- extract(rast0, coords[, c(which(colnames(coords) == 'lon'), which(colnames(coords) == 'lat'))])

  df_out <- data.frame(
    by = coords[[by]],
    val <- rast_vec
  )

  return(df_out)

}
