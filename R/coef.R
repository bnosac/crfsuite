
#' @export
coefficients.crf <- function(object, encoding = "unknown", ...){
  stopifnot(file.exists(object$file_model))
  x             <- crfsuite_model_coefficients(object$file_model)
  Encoding(x$transitions$from) <- encoding
  Encoding(x$transitions$to)   <- encoding
  Encoding(x$states$attribute) <- encoding
  Encoding(x$states$label)     <- encoding
  x$transitions <- data.table::setDF(x$transitions)
  x$states      <- data.table::setDF(x$states)
  x$transitions <- x$transitions[order(x$transitions$weight, decreasing = TRUE), c("from", "to", "weight")]
  x$states      <- x$states[order(x$states$weight, decreasing = TRUE), c("attribute", "label", "weight")]
  
  rownames(x$transitions) <- NULL
  rownames(x$states)      <- NULL
  x
}

#' @export
coef.crf <- function(object, encoding = "unknown", ...){
  coefficients.crf(object, encoding, ...)
}