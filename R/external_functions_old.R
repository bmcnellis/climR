#' @title Functions borrowed from other packages
#'
#' @description  NA
#'
#' @name external_functions
NULL
#' @rdname external_functions
#' @export
nc_get_variable_list <- function(f, min.dims=1) {

  # this is ncdf4.helpers::nc.get.variable.list, copied because it didn't play well with R 3.5.3
  var.list <- names(f$var)
  enough.dims <- sapply(var.list, function(v) { length(f$var[[v]]$dim) >= min.dims } )
  bounds <- nc_get_dim_bounds_var_list(f)
  climatology.bounds <- nc_get_climatology_bounds_var_list(f)
  has.axis <- unlist(lapply(var.list, function(x) { a <- ncdf4::ncatt_get(f, x, "axis"); if(a$hasatt & nchar(a$value) == 1) return(x); return(NULL); } ))

  ## When things get really broken, we'll need this...
  bnds.heuristic <- !grepl("_bnds", var.list)

  var.mask <- bnds.heuristic & enough.dims & (!(var.list %in% c(bounds, has.axis, climatology.bounds, "lat", "lon") | unlist(lapply(f$var, function(x) { return(x$prec == "char" | x$ndims == 0) }))))

  return(var.list[var.mask])

}
#' @rdname external_functions
#' @export
nc_get_dim_bounds_var_list <- function(f, v=NULL) {

  # this is needed to make nc_get_variable_list work
  dimension.vars <- names(f$dim)
  dim.names <- if(is.null(v)) names(f$dim) else nc.get.dim.names(f, v)
  return(unlist(sapply(names(f$dim), function(x) {
    if(f$dim[[x]]$create_dimvar) {
      a <- ncdf4::ncatt_get(f, x, "bounds");
      if(a$hasatt)
        return(a$value);
    }

    ## Heuristic detection for broken files
    bnds.vars <- c(paste(x, "bnds", sep="_"), paste("bounds", x, sep="_"))
    bnds.present <- bnds.vars %in% names(f$var)
    if(any(bnds.present))
      return(bnds.vars[bnds.present])

    return(NULL);
  } )))
}
#' @rdname external_functions
#' @export
nc_get_climatology_bounds_var_list <- function(f) {

  # this is needed to make nc_get_variable_list work
  dim.list <- names(f$dim)
  is.climatology<- sapply(dim.list, function(x) {
    if(f$dim[[x]]$create_dimvar && f$dim[[x]]$unlim) {
      a <- ncdf4::ncatt_get(f, x, "climatology")
      if(a$hasatt)
        return(a$value)
    }
    return(NA)
  })
  return(unique(is.climatology[!is.na(is.climatology)]))
}
