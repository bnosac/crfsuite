#' @title \code{NA} friendly version of sprintf
#' @description Does the same as the function \code{\link[base]{sprintf}} except that if 
#' in ... \code{NA} values are passed, also \code{NA} values are returned instead of being replaced by the character string \code{'NA'}.
#' @param fmt a character vector of format strings, which will be fed on to \code{\link[base]{sprintf}}
#' @param ... values to be passed into \code{fmt}, the \code{...} will be passed on to \code{\link[base]{sprintf}}
#' @return The same as what \code{\link[base]{sprintf}} returns:
#' a character vector of length that of the longest input in \code{...}. \cr
#' Except, in case any of the values passed on to \code{...} are \code{NA}, 
#' the corresponding returned value will be set to \code{NA} for that element of the vector. \cr
#' See the examples to see the difference with \code{\link[base]{sprintf}}
#' @export
#' @seealso \code{\link[base]{sprintf}}
#' @examples 
#' sprintf("(w-1):%s", c("xyz", NA, "abc"))
#' txt_sprintf("(w-1):%s", c("xyz", NA, "abc"))
#' sprintf("(w-1):%s_%s", c("xyz", NA, "abc"), c(NA, "xyz", "abc"))
#' txt_sprintf("(w-1):%s_%s", c("xyz", NA, "abc"), c(NA, "xyz", "abc"))
txt_sprintf <- function(fmt, ...){
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