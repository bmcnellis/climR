#' @title Miscellaneous supporting functions
#'
#' @description  NA
#'
#' @name misc
#' @author Brandon McNellis
NULL
#' @rdname misc
#' @export
std_err <- function(x, na.rm = F) {
  y <- sd(x, na.rm = na.rm) / sqrt(length(x))
  return(y)
}
#' @rdname misc
#' @export
trimws_df <- function(x, stringsAsFactors = F) {
  stopifnot(is.data.frame(x))
  y <- lapply(x, function(yy) {
    if (is.character(yy)) {
      zz <- trimws(yy)
    } else {
      zz <- yy
    }
    zz
  })

  z <- data.frame(y, stringsAsFactors = stringsAsFactors)
  z
}
#' @rdname misc
#' @export
fac_to_char <- function(x) {
  if (inherits(x, 'factor')) {
    y <- as.character(x)
  } else {
    y <- x
  }
  return(y)
}
#' @rdname misc
#' @export
mode0 <- function(x) {
  u0 <- unique(x)
  uu0 <- u0[which.max(tabulate(match(x, u0)))]
  return(uu0)

}
#' @rdname misc
#' @export
inv_logit <- function(x) {
  y <- exp(x) / (1 + exp(x))
  y
}
