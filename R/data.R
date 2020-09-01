#' @title CRF Training data: download training data for doing Named Entity Recognition (NER)
#' @description Download training data for doing Named Entity Recognition (NER)
#' @param type a character string with the type of data to download. See the function usage for all possible values.
#' These data will be downloaded from either:
#' \itemize{
#'  \item{NLTK-data forked repository: }{\url{https://github.com/bnosac-dev/nltk_data/blob/gh-pages/packages/corpora/conll2002.zip}}
#'  \item{FOX forked repository of GermanNER: }{\url{https://github.com/bnosac-dev/FOX/tree/master/input/GermanNER}}
#'  \item{FOX forked repository of WikiNER: }{\url{https://github.com/bnosac-dev/FOX/tree/master/input/Wikiner}}
#' }
#' Please visit the information on these repositories first before you use these data in any commercial product.
#' @param docs integer indicating how many documents to sample from the data (only used for data from the NLTK repository). 
#' This is only used to reduce CRAN R CMD check training time in the examples of this R package.
#' @return a data.frame with training data for a Named Entity Recognition task or an object of try-error in case of failure of downloading the data
#' @export
#' @examples 
#' \dontrun{
#' x <- ner_download_modeldata("conll2002-nl")
#' x <- ner_download_modeldata("conll2002-es")
#' x <- ner_download_modeldata("GermanNER")
#' x <- ner_download_modeldata("wikiner-en-wp2")
#' x <- ner_download_modeldata("wikiner-nl-wp3")
#' x <- ner_download_modeldata("wikiner-fr-wp3")
#' }
#' ## reduce number of docs
#' x <- ner_download_modeldata("conll2002-es", docs = 10)
ner_download_modeldata <- function(type = c("conll2002-nl", "conll2002-es", "GermanNER", 
                                            "wikiner-de-wp2",
                                            "wikiner-de-wp3",
                                            "wikiner-en-wp2",
                                            "wikiner-en-wp3",
                                            "wikiner-es-wp2",
                                            "wikiner-es-wp3",
                                            "wikiner-fr-wp2",
                                            "wikiner-fr-wp3",
                                            "wikiner-it-wp2",
                                            "wikiner-it-wp3",
                                            "wikiner-nl-wp2",
                                            "wikiner-nl-wp3",
                                            "wikiner-pl-wp3",
                                            "wikiner-pt-wp3",
                                            "wikiner-ru-wp2",
                                            "wikiner-ru-wp3"), 
                                   docs = -Inf){
  .N <- sentence_id <- doc_id <- txt <- NULL
  
  msg <- suppressWarnings(try({
    type <- match.arg(type)
    temporary_file <- tempfile()
    if(type == "conll2002-nl"){
      url <- "https://raw.githubusercontent.com/bnosac-dev/nltk_data/gh-pages/packages/corpora/conll2002.zip"
      download.file(url, temporary_file)
      rawdata <- list()
      f <- unz(temporary_file, filename = "conll2002/ned.train")
      rawdata$ned.train <- readLines(f, encoding = "UTF-8")
      close(f)
      f <- unz(temporary_file, filename = "conll2002/ned.testa")
      rawdata$testa <- readLines(f, encoding = "UTF-8")
      close(f) 
      f <- unz(temporary_file, filename = "conll2002/ned.testb")
      rawdata$testb <- readLines(f, encoding = "UTF-8")
      close(f)    
      rawdata <- lapply(rawdata, FUN=function(x){
        x <- data.frame(txt = x, stringsAsFactors = FALSE)
        x$doc_id <- cumsum(x$txt == "-DOCSTART- -DOCSTART- O")
        if(is.finite(docs) & docs > 0){
          d <- unique(x$doc_id)
          x <- x[x$doc_id %in% sample(d, size = min(docs, length(d))), ]
        }
        x$sentence_id <- cumsum(x$txt == "") + 1L
        x <- x[x$txt != "" & x$txt != "-DOCSTART- -DOCSTART- O", ]
        x$txt <- strsplit(x$txt, " ")
        x$token <- sapply(x$txt, FUN=function(x) x[1])
        x$pos <- sapply(x$txt, FUN=function(x) x[2])
        x$label <- sapply(x$txt, FUN=function(x) x[3])
        x[, c("doc_id", "sentence_id", "token", "pos", "label")]
      })
      rawdata <- data.table::rbindlist(rawdata, idcol = "data")
      rawdata$doc_id <- as.integer(factor(sprintf("%s-%s", rawdata$data, rawdata$doc_id)))
      file.remove(temporary_file)
    }else if(type == "conll2002-es"){
      url <- "https://raw.githubusercontent.com/bnosac-dev/nltk_data/gh-pages/packages/corpora/conll2002.zip"
      download.file(url, temporary_file)
      rawdata <- list()
      f <- unz(temporary_file, filename = "conll2002/esp.train")
      rawdata$train <- readLines(f, encoding = "UTF-8")
      close(f)
      f <- unz(temporary_file, filename = "conll2002/esp.testa")
      rawdata$testa <- readLines(f, encoding = "UTF-8")
      close(f) 
      f <- unz(temporary_file, filename = "conll2002/esp.testb")
      rawdata$testb <- readLines(f, encoding = "UTF-8")
      close(f) 
      rawdata <- lapply(rawdata, FUN=function(x){
        x <- data.frame(txt = x, stringsAsFactors = FALSE)
        x$doc_id <- cumsum(x$txt == "") + 1L
        if(is.finite(docs) & docs > 0){
          d <- unique(x$doc_id)
          x <- x[x$doc_id %in% sample(d, size = min(docs, length(d))), ]
        }
        x <- x[x$txt != "", ]
        x$txt <- strsplit(x$txt, " ")
        x$token <- sapply(x$txt, FUN=function(x) x[1])
        x$pos <- sapply(x$txt, FUN=function(x) x[2])
        x$label <- sapply(x$txt, FUN=function(x) x[3])
        x[, c("doc_id", "token", "pos", "label")]
      })
      rawdata <- data.table::rbindlist(rawdata, idcol = "data")
      rawdata$doc_id <- as.integer(factor(sprintf("%s-%s", rawdata$data, rawdata$doc_id)))
      file.remove(temporary_file)
    }else if(type == "GermanNER"){
      rawdata <- readLines("https://raw.githubusercontent.com/bnosac-dev/FOX/master/input/GermanNER/full_train.tsv", encoding = "UTF-8")
      rawdata <- data.frame(txt = rawdata, stringsAsFactors = FALSE)
      rawdata$doc_id <- cumsum(rawdata$txt == "") + 1L
      rawdata <- rawdata[rawdata$txt != "", ]
      rawdata$txt <- strsplit(rawdata$txt, "\t")
      rawdata$token <- sapply(rawdata$txt, FUN=function(x) x[1])
      rawdata$label <- sapply(rawdata$txt, FUN=function(x) x[2])
      rawdata <- rawdata[, c("doc_id", "token", "label")]
    }else if(type %in% c("wikiner-de-wp2",
                         "wikiner-de-wp3",
                         "wikiner-en-wp2",
                         "wikiner-en-wp3",
                         "wikiner-es-wp2",
                         "wikiner-es-wp3",
                         "wikiner-fr-wp2",
                         "wikiner-fr-wp3",
                         "wikiner-it-wp2",
                         "wikiner-it-wp3",
                         "wikiner-nl-wp2",
                         "wikiner-nl-wp3",
                         "wikiner-pl-wp3",
                         "wikiner-pt-wp3",
                         "wikiner-ru-wp2",
                         "wikiner-ru-wp3")){
      url <- sprintf("https://raw.githubusercontent.com/bnosac-dev/FOX/master/input/Wikiner/aij-%s.bz2", type)
      download.file(url, temporary_file)
      rawdata <- data.frame(txt = readLines(temporary_file, encoding = "UTF-8"), stringsAsFactors = FALSE)
      rawdata$doc_id <- cumsum(rawdata$txt == "")
      rawdata <- rawdata[rawdata$txt != "", ]
      rawdata <- data.table::setDT(rawdata)
      rawdata <- rawdata[, sentence_id := 1:.N, by = list(doc_id)]
      rawdata <- rawdata[, list(txt = unlist(strsplit(txt, " "))), by = list(doc_id, sentence_id)]
      rawdata <- rawdata[, c("token", "pos", "label") := tstrsplit(txt, "\\|")]
      rawdata <- data.table::setDF(rawdata)
      rawdata <- rawdata[, c("doc_id", "sentence_id", "token", "pos", "label")]
      file.remove(temporary_file)
    }
    setDT(rawdata)
  }, silent = TRUE))
  if(inherits(msg, "try-error")){
    return(msg)
  }
  rawdata
}



#' @title Dutch reviews of AirBnB customers on Brussels address locations available at www.insideairbnb.com
#' @description The data contains 500 reviews in Dutch of people who visited an AirBnB appartment in Brussels. \cr
#' The data frame contains the fields 
#' \itemize{
#'  \item{doc_id: }{a unique identifier of the review}
#'  \item{listing_id: }{the airbnb address identifier}
#'  \item{text: }{text with the feedback of a customer on his visit in the AirBnB appartment}
#' }
#' @name airbnb
#' @docType data
#' @source \url{http://data.insideairbnb.com/belgium/bru/brussels/2015-10-03/visualisations/reviews.csv}, \url{http://insideairbnb.com/get-the-data.html}
#' @seealso \code{\link{airbnb_chunks}}
#' @examples
#' data(airbnb)
#' str(airbnb)
#' head(airbnb)
NULL


#' @title Dutch reviews of AirBnB customers on Brussels address locations manually tagged with entities 
#' @description The \code{\link{airbnb}} dataset was manually annotated with the shiny app inside this R package.
#' The annotation shows chunks of data which have been flagged with the following categories: PERSON, LOCATION, DISTANCE.
#' The dataset is an object of class \code{chunkrange} and of type data.frame which contains the following fields:
#' \itemize{
#'  \item{doc_id: }{a unique identifier of the review, which is also available in \code{\link{airbnb}}}
#'  \item{listing_id: }{the airbnb address identifier}
#'  \item{text: }{text with the feedback of a customer on his visit in the AirBnB appartment}
#'  \item{chunk_id: }{a chunk identifier}
#'  \item{chunk_entity: }{a chunk entity label}
#'  \item{chunk: }{the text of the chunk which is a substring of \code{text}}
#'  \item{start: }{the starting position in \code{text} where the \code{chunk} is found}
#'  \item{end: }{the end position in \code{text} where the \code{chunk} is found}
#' }
#' @name airbnb_chunks
#' @docType data
#' @seealso \code{\link{airbnb_chunks}}
#' @examples
#' data(airbnb_chunks)
#' str(airbnb_chunks)
#' head(airbnb_chunks)
NULL


