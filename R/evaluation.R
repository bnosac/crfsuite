
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
#' @description The accuracy, precision, recall, specificity, F1 measure and support metrics are provided for each label in a one-versus the rest setting.
#' @param pred a factor with predictions 
#' @param obs a factor with gold labels
#' @param labels a character vector of possible values that \code{pred} and \code{obs} can take. Defaults to the values in the data
#' @param labels_overall a character vector of either labels which is either the same as \code{labels} or a subset of \code{labels} in order to compute a weighted average of the by-label statistics
#' @return a list with 2 elements:
#' \itemize{
#' \item{bylabel: data.frame with the accuracy, precision, recall, specificity, F1 score and support (number of occurrences) for each label}
#' \item{overall: a vector containing 
#' \itemize{
#' \item{the overall accuracy}
#' \item{the metrics precision, recall, specificity and F1 score which are weighted averages of these metrics from list element \code{bylabel}, where the weight is the support}
#' \item{the metrics precision, recall, specificity and F1 score which are averages of these metrics from list element \code{bylabel} giving equal weight to each label}
#' }
#' }
#' }
#' @export
#' @examples
#' pred <- sample(LETTERS, 1000, replace = TRUE)
#' gold <- sample(LETTERS, 1000, replace = TRUE)
#' crf_evaluation(pred = pred, obs = gold, labels = LETTERS) 
#' 
#' \donttest{
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
#' \dontshow{if(require(udpipe))\{}         
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
#' \dontshow{\} # End of main if statement running only if the required packages are installed}
#' }
crf_evaluation <- function(pred, obs, 
                           labels = na.exclude(unique(c(as.character(pred), as.character(obs)))),
                           labels_overall = setdiff(labels, "O")){
  levs <- na.exclude(unique(c(as.character(pred), as.character(obs))))
  levs <- setdiff(levs, labels)
  if(length(levs) > 0){
    stop(sprintf("More levels in the data then in labels, namely: %s", paste(levs, collapse = ", ")))
  }
  tab <- table(factor(pred, levels = labels), factor(obs, levels = labels), dnn = c("prediction", "gold"))
  
  result <- lapply(labels, FUN=function(positive){
    tab2d <- as_2d_table(tab, positive = positive)
    tab2d$accuracy <- ifelse(tab2d$total == 0, NA_real_, (tab2d$tp + tab2d$tn) / tab2d$total)
    tab2d$precision <- tab2d$tp / (tab2d$tp + tab2d$fp)
    tab2d$recall <- tab2d$tp / tab2d$p
    tab2d$f1 <- 1 / (0.5 * 1 / tab2d$precision + 0.5 * 1 / tab2d$recall)
    tab2d$specificity <- tab2d$tn / tab2d$n
    tab2d$support <- tab2d$p
    tab2d[c("accuracy", "precision", "recall", "specificity", "f1")] <- lapply(tab2d[c("accuracy", "precision", "recall", "specificity", "f1")], FUN=function(x) ifelse(is.infinite(x) | is.nan(x), NA_real_, x))
    tab2d <- tab2d[c("accuracy", "precision", "recall", "specificity", "f1", "support")]
  })
  names(result) <- labels
  result <- data.table::rbindlist(result, idcol = "label")
  result <- data.table::setDF(result)
  
  accuracy <- as.data.frame(tab, responseName = "n")
  overview <- result[result$label %in% labels_overall, ]
  overview <- c(
    accuracy = sum(accuracy$n[accuracy$prediction == accuracy$gold]) / sum(tab, na.rm=TRUE),
    precision = weighted.mean(overview$precision, w = overview$support, na.rm=TRUE),
    recall = weighted.mean(overview$recall, w = overview$support, na.rm=TRUE),
    specificity = weighted.mean(overview$specificity, w = overview$support, na.rm=TRUE),
    f1 = weighted.mean(overview$f1, w = overview$support, na.rm=TRUE),
    precision_mean = mean(overview$precision, na.rm=TRUE),
    recall_mean = mean(overview$recall, na.rm=TRUE),
    specificity_mean = mean(overview$specificity, na.rm=TRUE),
    f1_mean = mean(overview$f1, na.rm=TRUE))
  list(bylabel = result, overall = overview)
}
