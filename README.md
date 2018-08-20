# crfsuite - Labelling Sequential Data in Natural Language Processing

This repository contains an R package which is an Rcpp wrapper around the CRFsuite C/C++ library (https://github.com/chokkan/crfsuite).

- This allows useRs to fit a 1st-order linear-chain Markov **Conditional Random Field** model with dyad features and to apply it on existing data.
- The focus of the implementation is in the area of **Natural Language Processing** where this R package allows you to easily build and apply models for **named entity recognition, chunking, parts of speech tagging or classification** of any category you have in mind.

## Installation

For installing the development version of this package: `devtools::install_github("bnosac/crfsuite", build_vignettes = TRUE)`

## Example

```
library(crfsuite)
##
## Get some training data
##
x <- ner_download_modeldata("conll2002-nl")
crf_train <- subset(x, data == "ned.train")
crf_test <- subset(x, data == "testa")

##
## Build the CRF model
##
opts <- crf_options("lbfgs")
opts <- opts$default
opts$max_iterations <- 25
model <- crf(y = crf_train$label, x = crf_train[, c("token", "pos")], 
             group = crf_train$doc_id, 
             method = "lbfgs", options = opts, trace = FALSE) 
model
stats <- summary(model, "modeldetails.txt")
plot(stats$iterations$loss)

##
## Use the CRF model to label a sequence
##
scores <- predict(model, 
                  newdata = crf_test[, c("token", "pos")], group = crf_test$doc_id)
table(scores$label)
table(scores$label == crf_test$label)

##
## Get more statistics on how good the model is using the caret package
##
library(caret)
overview <- confusionMatrix(scores$label, crf_test$label, mode = "prec_recall")
overview$overall
overview$byClass[, c("Precision", "Recall", "F1")]
```

## Example with feature engineering

The following example includes features of the neighbouring tokens. Please visit the udpipe R package (https://CRAN.R-project.org/package=udpipe) for more information on how to extract e.g. parts of speech tags or other language features.

```
library(crfsuite)
library(udpipe)
library(data.table)
x <- ner_download_modeldata("conll2002-nl")
x <- setDT(x)
x <- x[, c("w[t-2]", "w[t-1]", "w[t]", "w[t+1]", "w[t+2]", 
           "w[t-2]|w[t-1]", "w[t-1]|w[t]", "w[t]|w[t+1]", "w[t+1]|w[t+2]") := list(
  txt_previous(x = token, n = 2),
  txt_previous(x = token, n = 1),
  token, 
  txt_next(x = token, n = 1),
  txt_next(x = token, n = 2), 
  txt_previous(x = txt_nextgram(x = token, n = 2, sep = "|"), n = 2),
  txt_previous(x = txt_nextgram(x = token, n = 2, sep = "|"), n = 1),
  txt_nextgram(x = token, n = 2, sep = "|"),
  txt_next(x = txt_nextgram(x = token, n = 2, sep = "|"), n = 1)),
  by = list(doc_id, sentence_id)]
x <- x[, c("pos[t-2]", "pos[t-1]", "pos[t]", "pos[t+1]", "pos[t+2]", 
           "pos[t-2]|pos[t-1]", "pos[t-1]|pos[t]", "pos[t]|pos[t+1]", "pos[t+1]|pos[t+2]") := list(
             txt_previous(x = pos, n = 2),
             txt_previous(x = pos, n = 1),
             pos, 
             txt_next(x = pos, n = 1),
             txt_next(x = pos, n = 2), 
             txt_previous(x = txt_nextgram(x = pos, n = 2, sep = "|"), n = 2),
             txt_previous(x = txt_nextgram(x = pos, n = 2, sep = "|"), n = 1),
             txt_nextgram(x = pos, n = 2, sep = "|"),
             txt_next(x = txt_nextgram(x = pos, n = 2, sep = "|"), n = 1)),
       by = list(doc_id, sentence_id)]
x <- setDF(x)


crf_train <- subset(x, data == "ned.train")
crf_test <- subset(x, data == "testa")
## Build the CRF model
attributes <- c("w[t-2]", "w[t-1]", "w[t]", "w[t+1]", "w[t+2]", 
                "w[t-2]|w[t-1]", "w[t-1]|w[t]", "w[t]|w[t+1]", "w[t+1]|w[t+2]",
                "pos[t-2]", "pos[t-1]", "pos[t]", "pos[t+1]", "pos[t+2]", 
                "pos[t-2]|pos[t-1]", "pos[t-1]|pos[t]", "pos[t]|pos[t+1]", "pos[t+1]|pos[t+2]")
model <- crf(y = crf_train$label, x = crf_train[, attributes], 
             group = crf_train$doc_id, 
             method = "lbfgs", options = list(max_iterations = 25), trace = TRUE) 
model
summary(model, "modeldetails.txt")

## Use the CRF model to label a sequence
scores <- predict(model, 
                  newdata = crf_test[, attributes], group = crf_test$doc_id)
library(caret)
overview <- confusionMatrix(scores$label, crf_test$label, mode = "prec_recall")
overview$overall
overview$byClass[, c("Precision", "Recall", "F1")]
```

## Support in text mining

Need support in text mining?
Contact BNOSAC: http://www.bnosac.be
