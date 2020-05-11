#' @title Functions for PRISM manipulation
#'
#' @description  NA
#'
#' @name PRISM
#' @author Brandon McNellis
NULL
#' @rdname PRISM
#' @export
extract_PRISM <- function(fl, coords, by) {
  stopifnot(
    require(raster),
    require(sp),
    file.exists(fl)
  )

  rast0 <- raster(fl)
  rast_vec <- extract(rast0, coords[, c(which(colnames(coords) == 'lon'), which(colnames(coords) == 'lat'))])

  df_out <- data.frame(by = coords[[by]], val = rast_vec, stringsAsFactors = F)
  colnames(df_out) <- c(by, 'val')

  return(df_out)

}
#' @rdname PRISM
#' @export
split_PRISM_fname <- function(fl_list, var = 2L) {
  stopifnot(is.character(fl_list), is.integer(var))

  l0 <- strsplit(fl_list, '_')
  l1 <- lapply(l0, function(x) x[var])
  unlist(l1)

}
