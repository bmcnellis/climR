#' @title Functions for gridMET/MACA manipulation
#'
#' @description  NA
#'
#' @name gridMET
#' @author Brandon McNellis
NULL
#' @rdname gridMET
#' @export
aggregate_gridMET_by_year <- function(data_dir, vars, yrs, FUN) {

  stopifnot(require(ncdf4))
  out_array  <- array()

  for (i in seq_along(vars)) {
    ii <- vars[i]
    cat('\nVar: ', ii)
    var_dir <- paste0(data_dir, '/', ii)
    fls <- list.files(var_dir)
    fli <- fls[grep(ii, fls)]

    for (j in seq_along(yrs)) {
      jj <- yrs[j]
      cat('\nYear: ', jj, '\n')
      flj <- fli[grep(jj, fli)]
      flj0 <- paste0(var_dir, '/', flj)

      stopifnot(length(flj) == 1, file.exists(flj0))

      ncj <- ncdf4::nc_open(flj0, write = F)
      vj <- nc_get_variable_list(ncj)

      nrow0 <- sapply(ncj$dim, function(x) x$len)['lon']
      ncol0 <- sapply(ncj$dim, function(x) x$len)['lat']
      nk <- matrix(nrow = nrow0, ncol = ncol0)

      if (all(i == 1, j == 1)) {
        out_array <- array(data = NA, dim = c(length(vars), length(yrs), nrow0, ncol0))
      }


      ind <- 0L
      for (k in seq(nrow(nk))) {
        for (q in seq(ncol(nk))) {

          ind <- ind + 1L
          ct_prog <- format(ind / length(nk) * 100, digits = 2, nsmall = 2)
          cat('\r', ct_prog, '%', rep(' ', 10))

          st <- c(k, q, 1)
          ct <- c(1, 1, -1)
          ncvarkq <- ncdf4::ncvar_get(nc = ncj, varid = vj, start = st, count = ct)

          if (all(is.na(ncvarkq))) {
            nk[k, q] <- NA
            next
          } else {
            kq0 <- do.call(what = 'FUN', args = list(x = ncvarkq))
            nk[k, q] <- kq0
          } # end ifelse

        } # end q
      } # end k
      cat('\n')

      nc_close(ncj)
      out_array[i, j, , ] <- nk
    } # end j
  } # end i

  return(out_array)
} # end function
#' @rdname gridMET
#' @export
subset_gridMET_by_latlon <- function(coords, by, data_dir, vars, yrs) {

  stopifnot(require(ncdf4))
  stopifnot(nrow(coords) > 0)
  stopifnot(all(c('lon', 'lat') %in% colnames(coords)))
  stopifnot(by %in% colnames(coords))

  out_array  <- array(dim = c(length(vars), length(yrs), nrow(coords), 365))

  ind <- 1L
  max_ind <- length(vars) * length(yrs) * nrow(coords)
  for (i in seq_along(vars)) {
    ii <- vars[i]
    cat('\n\nvar:', ii, '\n')
    var_dir <- paste0(data_dir, '/', ii)
    fls <- list.files(var_dir)
    fli <- fls[grep(ii, fls)]

    for (j in seq_along(yrs)) {
      jj <- yrs[j]
      cat('\nyear:', jj, '\n')
      flj <- fli[grep(jj, fli)]
      flj0 <- paste0(var_dir, '/', flj)

      stopifnot(length(flj) == 1, file.exists(flj0))

      ncj <- ncdf4::nc_open(flj0, write = F)
      vj <- nc_get_variable_list(ncj)

      lons <- ncj$dim$lon$vals
      lats <- ncj$dim$lat$vals

      for (k in seq(nrow(coords))) {
        ct_prog <- format((ind / max_ind) * 100, digits = 2, nsmall = 2)
        cat('\r', ct_prog, '%', rep(' ', 10))

        klon <- coords[k, 'lon']
        klat <- coords[k, 'lat']
        klon_ind <- which.min(abs(lons - klon))
        klat_ind <- which.min(abs(lats - klat))

        st <- c(klon_ind, klat_ind, 1)
        ct <- c(1, 1, -1)
        ncvark <- ncdf4::ncvar_get(nc = ncj, varid = vj, start = st, count = ct)

        if (length(ncvark) == 366) {
          ncvark <- ncvark[-60]
          out_array[i, j, k, ] <- ncvark
        } else if (length(ncvark) < 365) {

          l0 <- length(ncvark)

          pd0_split <- split(seq(365), ceiling(seq_along(seq(365)) / (365 / l0)))
          pd0_index <- sapply(pd0_split, function(x) x[length(x)])
          pd0_index <- pd0_index[seq(l0)]
          names(pd0_index) <- NULL

          pd0_data <- ncvark
          ncvark <- rep(NA, 365)
          ncvark[pd0_index] <- pd0_data

          out_array[i, j, k, ] <- ncvark

        } else if (length(ncvark) == 365) {
          out_array[i, j, k, ] <- ncvark
        }

        ind <- ind + 1L

      } # end k
    } # end j
  } # end i

  dimnames(out_array) <- list(
    vars = vars,
    years = as.character(yrs),
    group = coords[[by]],
    DOY = as.character(seq(365))
  )

  return(out_array)

} # end function
#' @rdname gridMET
#' @export
collapse_gridMET_subset <- function(subset_array, coords, by = 'PLOT', FUN = mean) {
  stopifnot(
    require(reshape2),
    length(dim(subset_array)) == 4,
    dim(subset_array)[3] == nrow(coords),
    by %in% colnames(coords)
  )

  arr0 <- apply(subset_array, c(1, 2, 3), FUN)
  arr1 <- apply(arr0, c(1, 3), FUN = function(x) mean(x, na.rm = T))

  arr_out <- reshape2::melt(arr1)

  return(arr_out)

}
