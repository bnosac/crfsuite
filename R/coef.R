#' @export
coefficients.crf <- function(object, ...){
  stopifnot(file.exists(object$file_model))
  crfsuite_model_coefficients(object$file_model)
}

#' @export
coef.crf <- function(object, ...){
  stopifnot(file.exists(object$file_model))
  crfsuite_model_coefficients(object$file_model)
}