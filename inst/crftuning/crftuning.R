library(caret)
library(crfsuite)

##
## Get training/test data
##
x         <- ner_download_modeldata("conll2002-nl", docs = 1000)
x$pos     <- txt_sprintf("Parts of Speech: %s", x$pos)
x$token   <- txt_sprintf("Token: %s", x$token)
crf_train <- subset(x, data == "ned.train", select = c("doc_id", "token", "pos", "label"))
crf_test  <- subset(x, data == "testa", select = c("doc_id", "token", "pos", "label"))
crf_train <- crf_cbind_attributes(crf_train, terms = c("token", "pos"), by = "doc_id")
crf_test  <- crf_cbind_attributes(crf_test, terms = c("token", "pos"), by = "doc_id")

## Have a look at all hyperparameters for each method
if(FALSE){
  # L-BFGS with L1/L2 regularization
  opts <- crf_options("lbfgs")
  View(opts$params)
  # SGD with L2-regularization
  opts <- crf_options("l2sgd")
  View(opts$params)
  # Averaged Perceptron
  opts <- crf_options("averaged-perceptron")
  View(opts$params)
  # Passive Aggressive
  opts <- crf_options("passive-aggressive")
  View(opts$params)
  # Adaptive Regularization of Weights (AROW)
  opts <- crf_options("arow")
  View(opts$params)
}

## Split in train/test according to doc_id
folds <- groupKFold(group = crf_train$doc_id, k = 2)
feats <- grep("token|pos", colnames(crf_train), value=TRUE)
tuning <- train(x = crf_train[, c("doc_id", feats)], y = crf_train$label, 
           method = crf_caretmethod[["lbfgs"]], 
           metric = "F1", maximize = TRUE,
           trControl = trainControl(index = folds, summaryFunction = function(data, ...){
             overview <- confusionMatrix(data$obs, data$pred, mode = "prec_recall")
             overview <- overview$byClass[, c("Precision", "Recall", "F1")]
             overview <- overview[!rownames(overview) %in% "Class: O", ]
             overview <- colMeans(overview, na.rm=TRUE)
             overview <- ifelse(is.na(overview), 0, overview)
             overview
           }),
           tuneGrid = expand.grid(method = "lbfgs", 
                                  max_iterations = 200,
                                  c1 = c(0, 1),
                                  c2 = c(0, 1),
                                  feature.minfreq = c(5), 
                                  feature.possible_states = FALSE,
                                  feature.possible_transitions = FALSE,
                                  num_memories = 6,
                                  epsilon = 0.000010,
                                  period = 10, delta = 0.000010, linesearch = "MoreThuente", max_linesearch = 20,
                                  stringsAsFactors = FALSE))

tuning$results
plot(tuning, metric = "F1")
plot(tuning, metric = "Recall")
plot(tuning, metric = "Precision")
