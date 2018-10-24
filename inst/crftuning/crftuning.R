library(caret)
library(crfsuite)

x         <- ner_download_modeldata("conll2002-nl", docs = 1000)
x$pos     <- txt_sprintf("Parts of Speech: %s", x$pos)
x$token   <- txt_sprintf("Token: %s", x$token)
crf_train <- subset(x, data == "ned.train", select = c("doc_id", "token", "pos", "label"))
crf_test  <- subset(x, data == "testa", select = c("doc_id", "token", "pos", "label"))

crf_caret <- list(library = "crfsuite", type = "Classification", 
                  parameters = data.frame(parameter = c("method", "max_iterations", "feature.minfreq"), 
                                          class = c("character", "integer", "integer"), 
                                          label = c("Training method", "Maximum number of iterations", "Minimum frequency of feature")),
                  grid = function(x, y, len = NULL, search = "grid"){
                    expand.grid(method = "lbfgs", max_iterations = c(5, 10, 20), stringsAsFactors = FALSE)
                  },
                  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...){
                    opts <- crf_options(method = param$method)$default
                    extraparams <- setdiff(names(param), "method")
                    extraparams <- intersect(extraparams, names(opts))
                    opts[extraparams] <- param[extraparams]
                    cat(sprintf("Setting: %s, %s", param$method, paste(mapply(extraparams, param[extraparams], FUN=function(key, value) paste(key, value, collapse = ": ")), collapse = ", "), sep = "\n"))
                    crf(x = x[, -1], y = as.character(y), group = x[, 1], method = param$method, options = opts, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL){
                    out <- predict(modelFit, newdata = newdata[, -1], group = newdata[, 1], type = "marginal")
                    out$label
                  },
                  prob = function(){
                    out <- predict(modelFit, newdata = newdata[, -1], group = newdata[, 1], type = "marginal")
                    out$marginal
                  })
folds <- groupKFold(group = crf_train$doc_id, k = 5)
tuning <- train(x = crf_train[, c("doc_id", "token", "pos")], y = crf_train$label, 
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
                                  max_iterations = c(10, 20, 50, 100, 200), 
                                  feature.minfreq = c(5, 10), 
                                  stringsAsFactors = FALSE))
plot(tuning, metric = "F1")
plot(tuning, metric = "Recall")
plot(tuning, metric = "Precision")

model <- crf(y = crf_train$label, 
             x = crf_train[, c("token", "pos")], 
             group = crf_train$doc_id, 
             method = "lbfgs", 
             options = list(max_iterations = 100, feature.minfreq = 5, c1 = 0, c2 = 1)) 
model
stats <- summary(model, "modeldetails.txt")
stats
plot(stats$iterations$loss)
