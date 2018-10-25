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

## Have a look at all hyperparameters
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

##
## Setup caret tuning method and tune some example hyperparameters
##
crf_caret <- list(library = "crfsuite", type = "Classification", 
                  parameters = data.frame(parameter = c("method", "feature.minfreq", "feature.possible_states", "feature.possible_transitions", "max_iterations", "c1", "c2"), 
                                          class = c("character", "integer", "logical", "logical", "integer", "numeric", "numeric"), 
                                          label = c("Training method", "Minimum frequency of feature", "Generate all possible state features", "Generate all possible transition features", "Maximum number of iterations", "L1 regularisation coefficient", "L2 regularisation coefficient")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    .NotYetImplemented()
                  },
                  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...){
                    opts <- crf_options(method = param$method)$default
                    extraparams <- setdiff(names(param), "method")
                    extraparams <- intersect(extraparams, names(opts))
                    opts[extraparams] <- param[extraparams]
                    cat(sprintf("Building model for Setting: %s, %s", param$method, paste(mapply(extraparams, param[extraparams], FUN=function(key, value) paste(key, value, sep = "=")), collapse = ", ")), sep = "\n")
                    crf(x = x[, -1], y = as.character(y), group = x[, 1], method = param$method, options = opts, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL){
                    predict(modelFit, newdata = newdata[, -1], group = newdata[, 1], type = "marginal")$label
                  },
                  prob = NULL)
folds <- groupKFold(group = crf_train$doc_id, k = 2)
feats <- grep("token|pos", colnames(crf_train), value=TRUE)
tuning <- train(x = crf_train[, c("doc_id", feats)], y = crf_train$label, 
           method = crf_caret, 
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
                                  c1 = c(0, 0.5, 1),
                                  c2 = c(0, 0.5, 1),
                                  feature.minfreq = c(5), 
                                  feature.possible_states = FALSE,
                                  feature.possible_transitions = FALSE,
                                  stringsAsFactors = FALSE))
tuning$results
plot(tuning, metric = "F1")
plot(tuning, metric = "Recall")
plot(tuning, metric = "Precision")

##
## Build a final model with the provided hyperparameters
##
model <- crf(y = crf_train$label, 
             x = crf_train[, c("token", "pos")], 
             group = crf_train$doc_id, 
             method = "lbfgs", 
             options = list(max_iterations = 100, feature.minfreq = 5, c1 = 0, c2 = 1)) 
model
stats <- summary(model, "modeldetails.txt")
stats
plot(stats$iterations$loss)
