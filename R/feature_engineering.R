#' @title NA friendly version of sprintf
#' @description Does the same as the function \code{\link[base]{sprintf}} except that if 
#' in ... NA values are passed, also NA values are returned.
#' @param fmt a character vector of format strings, which will be fed on to \code{\link[base]{sprintf}}
#' @param ... values to be passed into \code{fmt}, which will be passed on to \code{\link[base]{sprintf}}
#' @return A character vector of length that of the longest input. Same as what \code{\link[base]{sprintf}}.
#' In case any of the values passed on to \code{...}, the corresponding returned value will be set to NA.
#' @export
#' @examples 
#' sprintf("(w-1):%s", c("xyz", NA, "abc"))
#' sprintf_na("(w-1):%s", c("xyz", NA, "abc"))
#' sprintf("(w-1):%s_%s", c("xyz", NA, "abc"), c(NA, "xyz", "abc"))
#' sprintf_na("(w-1):%s_%s", c("xyz", NA, "abc"), c(NA, "xyz", "abc"))
sprintf_na <- function(fmt, ...){
  x <- sprintf(fmt = fmt, ...)
  ldots <- list(...)
  if(length(ldots) > 0){
    ## Look to the longest element of ... first and next check if it should be NA
    inputi <- order(sapply(ldots, length), decreasing = TRUE)  
    make_na <- is.na(ldots[[inputi[1]]])
    if(length(ldots) >= 2){
      for(i in inputi[-1]){
        make_na <- make_na | is.na(ldots[[i]])
      }
    }
    x[make_na] <- NA_character_
  }
  x
}