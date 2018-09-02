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


#' @title Enrich a data.frame by adding frequently used CRF attributes
#' @description The CRF attributes which are implemented in this function 
#' are merely the neighbouring information of a certain field.
#' For example the previous word, the next word, the combination of the previous 2 words.
#' This function \code{cbind}s these neighbouring attributes as columns to the provided data.frame.\cr
#' 
#' By default it adds the following columns to the data.frame 
#' \itemize{
#'  \item{the term itself \code{(term[t])}}
#'  \item{the next term \code{(term[t+1])}}
#'  \item{the term after that \code{(term[t+2])}}
#'  \item{the previous term \code{(term[t-1])}}
#'  \item{the term before the previous term \code{(term[t-2])}}
#'  \item{as well as all combinations of these terms (bigrams/trigrams/...) where up to \code{ngram_max}
#' number of terms are combined.}
#' }
#' See the examples.
#' @param data a data.frame which will be coerced to a data.table (cbinding will be done by reference on the existing data.frame)
#' @param terms a character vector of column names which are part of \code{data} 
#' for which the function will look to the preceding and following rows in order to cbind this information to the \code{data}
#' @param by a character vector of column names which are part of \code{data} indicating the fields which define the sequence. 
#' Preceding/following terms will be looked for within data of \code{by}. 
#' Typically this will be a document identifier or sentence identifier in an NLP context.
#' @param from integer, by default set to -2, indicating to look up to 2 terms before the current term
#' @param to integer, by default  set to 2, indicating to look up to 2 terms after the current term
#' @param ngram_max integer indicating the maximum number of terms to combine (2 means bigrams, 3 trigrams, ...)
#' @param sep character indicating how to combine the previous/next/current terms. Defaults to '-'.
#' @export
#' @examples 
#' x <- data.frame(doc_id = sort(sample.int(n = 10, size = 1000, replace = TRUE)))
#' x$pos <- sample(c("Art", "N", "Prep", "V", "Adv", "Adj", "Conj", 
#'                   "Punc", "Num", "Pron", "Int", "Misc"), 
#'                   size = nrow(x), replace = TRUE)
#' x <- crf_cbind_attributes(x, terms = "pos", by = "doc_id", 
#'                           from = -1, to = 1, ngram_max = 3)
#' head(x)
#' 
#' \dontrun{
#' ## Example on some real data
#' x <- ner_download_modeldata("conll2002-nl")
#' x <- crf_cbind_attributes(x, terms = c("token", "pos"), 
#'                           by = c("doc_id", "sentence_id"),
#'                           ngram_max = 3, sep = "|")
#' }
crf_cbind_attributes <- function(data, terms, by, from = -2, to = 2, ngram_max = 3, sep = "-"){
  for(column in terms){
    data <- crf_cbind_attributes_single(data = data, field = column, by = by, from = from, to = to, ngram_max = ngram_max, sep = sep)
  }
  data
}


crf_cbind_attributes_single <- function(data, field, by, from = -2, to = 2, ngram_max = 3, sep = "-"){
  .SD <- .N <- NULL
  stopifnot(inherits(data, "data.frame"))
  stopifnot(is.character(field))
  stopifnot(length(field) == 1)
  stopifnot(all(by %in% colnames(data)))
  stopifnot(all(field %in% colnames(data)))
  ngram_max <- as.integer(ngram_max)
  stopifnot(ngram_max >= 1)
  data <- setDT(data)
  
  posprocessfields <- c()
  ##
  ## LEVEL 1: just term itself and terms in the neighbourhood (previous/next term)
  ## term itself, look ahead and look back
  elements <- seq(from, to, by = 1L)
  level1 <- c()
  
  look <- elements[elements < 0]
  newfields <- sprintf("%s[t%s]", field, look)
  level1 <- append(level1, newfields)
  data[, c(newfields) := Map(x = .SD, n = look, f=function(x, n) data.table::shift(x, n = -n, type = "lag")), by = by, .SDcols = rep(field, length(newfields))]
  look <- 0
  newfields <- sprintf("%s[t]", field)
  level1 <- append(level1, newfields)
  data[, c(newfields) := Map(x = .SD, n = look, f=function(x, n) x), by = by, .SDcols = rep(field, length(newfields))]
  look <- elements[elements > 0]
  newfields <- sprintf("%s[t+%s]", field, look)
  level1 <- append(level1, newfields)
  data[, c(newfields) := Map(x = .SD, n = look, f=function(x, n) data.table::shift(x, n = n, type = "lead")), by = by, .SDcols = rep(field, length(newfields))]
  
  posprocessfields <- append(posprocessfields, level1)
  
  ##
  ## Higher levels, get all combinations of fields + paste the fields together using the separator
  ##
  for(n in 2:ngram_max){
    combinations <- combn(level1, m = n, simplify=FALSE)
    for(fields in combinations){
      newfield <- paste(fields, collapse = sep)
      fmt <- paste(rep("%s", length(fields)), collapse = sep)
      addfmt <- function(x, fmt){
        x$fmt <- fmt
        x
      }
      data[, c(newfield) := do.call(txt_sprintf, addfmt(.SD, fmt)), .SDcols = fields]
      posprocessfields <- append(posprocessfields, newfield)
    }
  }
  
  ##
  ## Add also column name as for CRFsuite all attributes are the same, this allows to distinguish the attributes among the columns
  ##
  for(newfield in posprocessfields){
    data[, c(newfield) := txt_sprintf("%s=%s", newfield, unlist(.SD)), .SDcols = newfield]
  }
  setDF(data)
}
