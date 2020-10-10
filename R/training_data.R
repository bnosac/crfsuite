

#' @title CRF Training data construction: add chunk entity category to a tokenised dataset
#' @description Chunks annotated with the shiny app in this R package indicate for a chunk of text of a document
#' the entity that it belongs to. As text chunks can contains several words, we need to have a way in
#' order to add this chunk category to each word of a tokenised dataset. That's what this function is doing.\cr
#' If you have a tokenised data.frame with one row per token/document which indicates the start and end position
#' where the token is found in the text of the document, this function allows to assign the chunk label to each token 
#' of the document.
#' @param x an object of class \code{chunkrange}. A \code{chunkrange} is just a data.frame which contains 
#' one row per chunk/doc_id. It should have the columns doc_id, text, chunk_id, chunk_entity, start and end.\cr
#' The fields \code{start} and \code{end} indicate in the original \code{text} where the chunks of words starts and where it ends. 
#' The \code{chunk_entity} is a label you have assigned to the chunk (e.g. ORGANISATION / LOCATION / MONEY / LABELXYZ / ...).
#' @param y a tokenised data.frame containing one row per doc_id/token It should have the columns \code{doc_id}, \code{start} and \code{end} where
#' the fields \code{start} and \code{end} indicate the positions in the original text of the \code{doc_id} where the token starts and where it ends. 
#' See the examples.
#' @param by.x a character string of a column of \code{x} which is an identifier which defines the sequence. Defaults to 'doc_id'.
#' @param by.y a character string of a column of \code{y} which is an identifier which defines the sequence. Defaults to 'doc_id'.
#' @param default_entity character string with the default \code{chunk_entity} to be assigned to the token if the token is not part of any chunk range.
#' Defaults to 'O'.
#' @param ... not used
#' @return the data.frame \code{y} where 2 columns are added, namely:
#' \itemize{
#'  \item{chunk_entity: The chunk entity of the token if the token is inside the chunk defined in \code{x}. If the token is not part of any chunk, the chunk category will be set to the \code{default} value.}
#'  \item{chunk_id: The chunk identifier of the chunk for which the token is inside the chunk.}
#' }
#' @export
#' @examples 
#' \donttest{
#' \dontshow{if(require(udpipe))\{}
#' library(udpipe)
#' udmodel <- udpipe_download_model("dutch-lassysmall")
#' if(packageVersion("udpipe") >= "0.7"){
#'   data(airbnb_chunks, package = "crfsuite")
#'   airbnb_chunks <- head(airbnb_chunks, 20)
#'   airbnb_tokens <- unique(airbnb_chunks[, c("doc_id", "text")])
#' 
#'   airbnb_tokens <- udpipe(airbnb_tokens, object = udmodel)
#'   head(airbnb_tokens)
#'   head(airbnb_chunks)
#' 
#'   ## Add the entity of the chunk to the tokenised dataset
#'   x <- merge(airbnb_chunks, airbnb_tokens)
#'   x[, c("doc_id", "token", "chunk_entity")]
#'   table(x$chunk_entity)
#' }
#' 
#' ## cleanup for CRAN
#' file.remove(udmodel$file_model)
#' \dontshow{\} # End of main if statement running only if the required packages are installed}
#' }
merge.chunkrange <- function(x, y, by.x = "doc_id", by.y = "doc_id", default_entity = "O", ...){
  chunk_entity <- doc_id <- misc <- sentence_id <- token <- token_id <- NULL
  stopifnot(inherits(x, "data.frame"))
  stopifnot(inherits(y, "data.frame"))
  stopifnot(all(c(by.x, "start", "end", "chunk_entity", "chunk_id") %in% colnames(x)))
  if(!all(c("start", "end") %in% colnames(y))){
    ## not start/end field in the annotated dataset, maybe this is because it was a udpipe annotation (from before version 0.7), 
    ## in that case, add it to the dataset based on the misc field, it therefore needs the columns sentence_id/token/token_id/misc
    if(all(c("doc_id", "sentence_id", "token", "token_id", "misc") %in% colnames(y))){
      y <- as.data.table(y)
      y[, c("start", "end") := udpipe_reconstruct(sentence_id = sentence_id, token = token, token_id = token_id, misc = misc, only_from_to = TRUE), 
          by = list(doc_id)]
      y <- as.data.frame(y)
    }
  }
  stopifnot(all(c(by.y, "start", "end") %in% colnames(y)))
  stopifnot(length(by.x) == 1)
  stopifnot(length(by.y) == 1)
  ## Only for overlapping documents, the chunk_entity and chunk_id will be added
  docs <- intersect(unique(y[[by.y]]), unique(x[[by.x]]))
  tokensdf <- y[y[[by.y]] %in% docs, ]
  tokensdf <- split(tokensdf, factor(tokensdf[[by.y]], levels = docs))
  chunks <- x[x[[by.x]] %in% docs, ]
  chunks <- split(chunks, factor(chunks[[by.x]], levels = docs))
  ## Loop over all documents containing the tokenised data + the chunks
  ## see if token is within the chunk (by looking if start/end location of the token is within the start/end location of the chunk)
  ## if the token is within the chunk, use the chunk label, if not use the default label
  result <- mapply(chunks = chunks,
                   tokenised = tokensdf, 
                   FUN = function(chunks, tokenised){
                     results <- mapply(
                       start = tokenised$start, 
                       end = tokenised$end, 
                       FUN=function(start, end, chunks){
                         ## is the token within the chunk
                         idx <- which(chunks$start <= start & chunks$end >= end)
                         ## if the token appears to be part of several chunks take the last data as the flexdashboard just put it below each other
                         ## and the latest assignment the the user provided will be the correct one
                         idx <- tail(idx, 1)
                         if(length(idx) == 0){
                           return(list(chunk_id = NA_integer_, chunk_entity = default_entity))
                          }else{
                            return(list(chunk_id = chunks$chunk_id[idx], chunk_entity = chunks$chunk_entity[idx]))
                          }
                         }, MoreArgs = list(chunks = chunks), SIMPLIFY = FALSE)
                     tokenised$chunk_entity <- sapply(results, FUN=function(x) x$chunk_entity)
                     tokenised$chunk_id <- sapply(results, FUN=function(x) x$chunk_id)
                     tokenised
                   }, SIMPLIFY = FALSE)
  result <- data.table::rbindlist(result)
  ## recode to IOB tagging scheme
  result <- result[chunk_entity != 'O', chunk_entity := ifelse(seq_along(chunk_entity) == 1L, sprintf("B-%s", chunk_entity), sprintf("I-%s", chunk_entity)), by = c(by.y, "chunk_id")] 
  result <- data.table::setDF(result)
  result
}


udpipe_reconstruct <- function(sentence_id, token, token_id, misc, only_from_to = FALSE){
  
  ##
  ## FROM THE UDPIPE DOCS: 
  ##
  
  # The markup uses the following MISC fields on tokens (not words in multi-word tokens):
  # SpacesBefore=content (by default empty): spaces/other content preceding the token
  # SpacesAfter=content (by default a space if SpaceAfter=No feature is not present, empty otherwise): spaces/other content following the token
  # SpacesInToken=content (by default equal to the FORM of the token): FORM of the token including original spaces (this is needed only if tokens are allowed to contain spaces and a token contains a tab or newline characters)
  
  # The content of all the three fields must be escaped to allow storing tabs and newlines. The following C-like schema is used:
  # \s: space
  # \t: tab
  # \r: CR character
  # \n: LF character
  # \p: | (pipe character)
  # \\: \ (backslash character)
  
  rawtxt <- token
  
  has_spacesafter_no <- grepl(pattern = "SpaceAfter=No", misc)
  has_spacesafter <- grepl(pattern = "SpacesAfter=", misc)
  has_spacesbefore <- grepl(pattern = "SpacesBefore=", misc)
  has_spacesintoken <- grepl(pattern = "SpacesInToken=", misc)
  
  ##
  ## Spaces after
  ##
  after <- rep("", length(token))
  ## if no spaceafter feature, there is a space
  after[!has_spacesafter] <- " "
  ## if missing, there is a space after
  after[is.na(misc)] <- " "
  ## if contains SpaceAfter=No, there is nothing to add
  after[has_spacesafter_no] <- ""
  ## if contains SpacesAfter=, add the spaces to the after part
  idx <- which(has_spacesafter)
  addme <- gsub(pattern = "(SpacesAfter=)(.+)($|Spaces)", "\\2", misc[idx])
  addme <- gsub("\\\\s", " ", addme)
  addme <- gsub("\\\\n", "\n", addme)
  addme <- gsub("\\\\t", "\t", addme)
  addme <- gsub("\\\\r", "\r", addme)
  addme <- gsub("\\\\p", "|", addme)
  addme <- gsub("\\\\", "\\", addme)
  after[idx] <- addme
  ## Fix for using std::istringstream in udpipe_annotate as it always ends with a newline character
  after[length(after)] <- gsub("\n$", "", after[length(after)])
  
  ##
  ## Spaces before
  ##
  before <- rep("", length(token))
  ## if contains SpacesBefore=, add the spaces to the after part
  idx <- which(has_spacesbefore)
  addme <- gsub(pattern = "(SpacesBefore=)(.+)($|Spaces)", "\\2", misc[idx])
  addme <- gsub("\\\\s", " ", addme)
  addme <- gsub("\\\\n", "\n", addme)
  addme <- gsub("\\\\t", "\t", addme)
  addme <- gsub("\\\\r", "\r", addme)
  addme <- gsub("\\\\p", "|", addme)
  addme <- gsub("\\\\", "\\", addme)
  before[idx] <- addme
  
  ##
  ## SpacesInToken - MISC field stores form of the token including original spaces if there is a space in the token which can not be handled by FORM
  ##
  idx <- which(has_spacesintoken)
  token[idx] <- gsub(pattern = "(SpacesInToken=)(.+)($|Spaces)", "\\2", misc[idx])
  
  ##
  ## Construct original text
  ##
  original_txt <- sprintf("%s%s%s", before, token, after)
  
  ##
  ## Multi-word tokens are not considered
  ##
  is_multi_word <- grepl("-", token_id)
  ids <- sprintf("%s.%s", sentence_id, token_id)
  ids_remove <- mapply(sentence_id = sentence_id[is_multi_word],
                       token_id = token_id[is_multi_word], 
                       FUN=function(sentence_id, token_id){
                         sprintf("%s.%s", sentence_id, unlist(strsplit(token_id, split = "-")))
                       }, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  idx <- which(ids %in% ids_remove)
  original_txt[idx] <- ""
  
  ##
  ## Construct from-to
  ##
  before[idx] <- ""
  after[idx] <- ""
  
  nchars <- nchar(original_txt)
  original_to <- cumsum(nchars)
  original_from <- original_to - nchars + 1L
  from <- original_from + nchar(before)
  to <- original_to - nchar(after)
  from[idx] <- NA_integer_
  to[idx] <- NA_integer_
  
  
  if(only_from_to){
    return(list(from = from, to = to))  
  }else{
    return(list(text = paste(original_txt, collapse = ""),
                from = from,
                to = to))  
  }
}