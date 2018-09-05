

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
#' data(airbnb, package = "crfsuite")
#' data(airbnb_chunks, package = "crfsuite")
#' data(airbnb_tokens, package = "crfsuite")
#' head(airbnb_tokens)
#' head(airbnb_chunks)
#' 
#' ## Add the entity of the chunk to the tokenised dataset
#' x <- merge(airbnb_chunks, airbnb_tokens)
#' table(x$chunk_entity)
#' 
#' 
#' \dontrun{
#' ##
#' ## Same but instead of loading the data, 
#' ## use the udpipe R package version >= 0.7 to tokenise the text
#' ##
#' library(udpipe)
#' udmodel <- udpipe_download_model("dutch", 
#'                                  udpipe_model_repo = "bnosac/udpipe.models.ud")
#' udmodel <- udpipe_load_model(udmodel$file_model)
#' x <- udpipe_annotate(udmodel, x = airbnb$text, doc_id = airbnb$doc_id)
#' x <- as.data.frame(x, detailed = TRUE)
#' x <- merge(airbnb_chunks, x)
#' table(x$chunk_entity)
#' }
merge.chunkrange <- function(x, y, by.x = "doc_id", by.y = "doc_id", default_entity = "O", ...){
  chunk_entity <- NULL
  stopifnot(inherits(x, "data.frame"))
  stopifnot(inherits(y, "data.frame"))
  stopifnot(all(c(by.x, "start", "end", "chunk_entity", "chunk_id") %in% colnames(x)))
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
  result <- result[chunk_entity != '0', chunk_entity := ifelse(seq_along(chunk_entity) == 1L, sprintf("B-%s", chunk_entity), sprintf("I-%s", chunk_entity)), by = c(by.y, "chunk_id")] 
  result <- data.table::setDF(result)
  result
}
