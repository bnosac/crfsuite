


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
#' \donttest{
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

#' @title Extract basic text features which are useful for entity recognition
#' @description Extract basic text features which are useful for entity recognition
#' @param x a character vector
#' @param type a character string, which can be one of 'is_capitalised', 'is_url', 'is_email', 'is_number', 'prefix', 'suffix'
#' @param n for type 'prefix' or 'suffix', the number of characters of the prefix/suffix
#' @return 
#' For type 'is_capitalised', 'is_url', 'is_email', 'is_number': a logical vector of the same length as \code{x}, indicating if \code{x} is capitalised, a url, an email or a number\cr
#' For type 'prefix', 'suffix': a character vector of the same length as \code{x}, containing the prefix or suffix \code{n} number of characters of \code{x}
#' @export
#' @examples 
#' txt_feature("Red Devils", type = "is_capitalised")
#' txt_feature("red devils", type = "is_capitalised")
#' txt_feature("http://www.bnosac.be", type = "is_url")
#' txt_feature("info@google.com", type = "is_email")
#' txt_feature("hi there", type = "is_email")
#' txt_feature("1230000", type = "is_number")
#' txt_feature("123.15", type = "is_number")
#' txt_feature("123,15", type = "is_number")
#' txt_feature("123abc", type = "is_number")
#' txt_feature("abcdefghijklmnopqrstuvwxyz", type = "prefix", n = 3)
#' txt_feature("abcdefghijklmnopqrstuvwxyz", type = "suffix", n = 3)
txt_feature <- function(x, type = c('is_capitalised', 'is_url', 'is_email', 'is_number', 'prefix', 'suffix'), n = 4){
  type <- match.arg(type)
  missing <- is.na(x)
  miss <- as.logical(NA)
  if(type == "is_capitalised"){
    result <- grepl("^[[:upper:]]", x)
  }else if(type == "is_url"){
    result <- grepl(pattern = "https?", x = x, ignore.case = TRUE)
  }else if(type == "is_email"){
    result <- grepl(".+@.+\\.", x)
  }else if(type == "is_number"){
    result <- grepl("^[[:digit:].,]+$", x)
  }else if(type == "prefix"){
    result <- substr(x, start = 1, stop = n)
  }else if(type == "suffix"){
    x_size <- nchar(x)
    start <- x_size - (n - 1)
    result <- substr(x, start = ifelse(start < 1, 1, start), stop = x_size)
  }
  result[missing] <- miss
  result
}