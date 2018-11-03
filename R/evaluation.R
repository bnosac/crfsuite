
as_2d_table <- function(x, positive){
  # in rows we put the prediction, in columns the real known gold data
  d2 <- matrix(NA_integer_, nrow = 2, ncol = 2, dimnames = list(prediction = c("positive", "negative"), reality = c("positive", "negative")))
  d2 <- as.table(d2)
  levs_positive <- intersect(colnames(x), positive)
  levs_negative <- setdiff(colnames(x), positive)
  d2["positive", "positive"] <- sum(x[levs_positive, levs_positive])
  d2["negative", "positive"] <- sum(x[levs_negative, levs_positive])
  d2["positive", "negative"] <- sum(x[levs_positive, levs_negative])
  d2["negative", "negative"] <- sum(x[levs_negative, levs_negative])
  d2[is.na(d2) | !is.finite(d2)] <- 0L
  out <- list(data = d2,
              total = sum(d2),
              p = sum(d2[, "positive"]),
              n = sum(d2[, "negative"]),
              tp = d2["positive", "positive"],
              fn = d2["negative", "positive"],
              fp = d2["positive", "negative"],
              tn = d2["negative", "negative"])
  out
}

#' @title Basic classification evaluation metrics for multi-class labelling
#' @description The precision, recall, F1 measure and support metrics are provided for each label in a one-versus rest setting.
#' @param pred a factor with predictions 
#' @param obs a factor with gold labels
#' @param labels a character vector of possible values that \code{pred} and \code{obs} can take. Defaults to the values in the data
#' @return a data.frame with the precision, recall, F1 score and support for each label
#' @export
#' @examples
#' pred <- sample(LETTERS, 1000, replace = TRUE)
#' gold <- sample(LETTERS, 1000, replace = TRUE)
#' crf_evaluation(pred = pred, obs = gold, labels = LETTERS) 
#' 
#' \dontrun{
#' x <- ner_download_modeldata("conll2002-nl")
#' x <- crf_cbind_attributes(x, terms = c("token", "pos"), 
#'                           by = c("doc_id", "sentence_id"))
#' crf_train <- subset(x, data == "ned.train")
#' crf_test <- subset(x, data == "testa")
#' attributes <- grep("token|pos", colnames(x), value=TRUE)
#' model <- crf(y = crf_train$label, 
#'              x = crf_train[, attributes], 
#'              group = crf_train$doc_id, 
#'              method = "lbfgs") 
#'              
#' ## Use the model to score on existing tokenised data
#' scores <- predict(model, 
#'                   newdata = crf_test[, attributes], 
#'                   group = crf_test$doc_id)
#' crf_evaluation(pred = scores$label, obs = crf_test$label)
#' crf_evaluation(pred = scores$label, obs = crf_test$label, 
#'   labels = c("O", 
#'              "B-ORG", "I-ORG", "B-PER", "I-PER", 
#'              "B-LOC", "I-LOC", "B-MISC", "I-MISC"))
#'              
#'              
#' library(udpipe)
#' pred <- txt_recode(scores$label, 
#'                    from = c("B-ORG", "I-ORG", "B-PER", "I-PER", 
#'                             "B-LOC", "I-LOC", "B-MISC", "I-MISC"),
#'                    to = c("ORG", "ORG", "PER", "PER", 
#'                           "LOC", "LOC", "MISC", "MISC"))
#' obs <- txt_recode(crf_test$label, 
#'                   from = c("B-ORG", "I-ORG", "B-PER", "I-PER", 
#'                            "B-LOC", "I-LOC", "B-MISC", "I-MISC"),
#'                   to = c("ORG", "ORG", "PER", "PER", 
#'                          "LOC", "LOC", "MISC", "MISC"))
#' crf_evaluation(pred = pred, obs = obs, 
#'                labels = c("ORG", "LOC", "PER", "MISC", "O"))
#' }
crf_evaluation <- function(pred, obs, labels = na.exclude(unique(c(pred, obs)))){
  levs <- na.exclude(unique(c(pred, obs)))
  levs <- setdiff(levs, labels)
  if(length(levs) > 0){
    stop(sprintf("More levels in the data then in labels, namely: %s", paste(levs, collapse = ", ")))
  }
  tab <- table(factor(pred, levels = labels), factor(obs, levels = labels), dnn = c("prediction", "gold"))
  
  result <- lapply(labels, FUN=function(positive){
    tab2d <- as_2d_table(tab, positive = positive)
    #tab2d$accuracy <- ifelse(tab2d$total == 0, NA_real_, (tab2d$tp + tab2d$tn) / tab2d$total)
    tab2d$precision <- tab2d$tp / (tab2d$tp + tab2d$fp)
    tab2d$recall <- tab2d$tp / tab2d$p
    tab2d$f1 <- 1 / (0.5 * 1 / tab2d$precision + 0.5 * 1 / tab2d$recall)
    tab2d$support <- tab2d$p
    tab2d[c("precision", "recall", "f1")] <- lapply(tab2d[c("precision", "recall", "f1")], FUN=function(x) ifelse(is.infinite(x), NA_real_, x))
    tab2d <- tab2d[c("precision", "recall", "f1", "support")]
  })
  names(result) <- labels
  result <- data.table::rbindlist(result, idcol = "label")
  result <- data.table::setDF(result)
  result
}
