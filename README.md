# Labelling Sequential Data in Natural Language Processing

This repository contains an R package wrapping the CRFsuite C/C++ library (https://github.com/chokkan/crfsuite). It allows

- R users to fit a 1st-order linear-chain Markov **Conditional Random Field** model with dyad features and to apply it on existing data.
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
             method = "l2sgd", trace = FALSE) 
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

In CRF models, the labels (y) obey the Markov property with respect to the graph conditional on the CRF attributes (x). In order to construct attributes (x), one looks to neighbouring elements of the tokens (for example 2 words before/after the token). The following example includes features of the neighbouring tokens (namely neighbouring terms and neighbouring parts of speech tags). 

Please visit the udpipe R package (https://CRAN.R-project.org/package=udpipe) for more information on how to extract e.g. parts of speech tags or other language features if you want to use these to extract the features of the word neighbours.

```
library(crfsuite)
library(udpipe)
library(data.table)

##
## Get some training data
##
x <- ner_download_modeldata("conll2002-nl")
x <- setDT(x)

##
## Add attributes of each sequence element
##

## Indicate beginning of sequence and end of sequence and sequence position
x <- x[, bos := ifelse(1:.N == 1, "BOS", sprintf("BOS+%s", (1:.N)-1)), by = list(doc_id, sentence_id)]
x <- x[, eos := ifelse(1:.N == .N, "EOS", sprintf("EOS-%s", (.N:1)-1)), by = list(doc_id, sentence_id)]

## Add preceding and next tokens and parts of speech tags
x <- crf_cbind_attributes(x, field = c("token", "pos"), by = c("doc_id", "sentence_id"), ngram_max = 3, sep = "|")

## Build the CRF model
crf_train <- subset(x, data == "ned.train")
crf_test <- subset(x, data == "testa")

attributes <- c("bos", "eos", 
                "token[t-2]", "token[t-1]", "token[t]", "token[t+1]", "token[t+2]", 
                "token[t-2]|token[t-1]", "token[t-2]|token[t]", 
                "token[t-2]|token[t+1]", "token[t-2]|token[t+2]", "token[t-1]|token[t]", 
                "token[t-1]|token[t+1]", "token[t-1]|token[t+2]", "token[t]|token[t+1]", 
                "token[t]|token[t+2]", "token[t+1]|token[t+2]", "token[t-2]|token[t-1]|token[t]", 
                "token[t-2]|token[t-1]|token[t+1]", "token[t-2]|token[t-1]|token[t+2]", 
                "token[t-2]|token[t]|token[t+1]", "token[t-2]|token[t]|token[t+2]", 
                "token[t-2]|token[t+1]|token[t+2]", "token[t-1]|token[t]|token[t+1]", 
                "token[t-1]|token[t]|token[t+2]", "token[t-1]|token[t+1]|token[t+2]", 
                "token[t]|token[t+1]|token[t+2]", "pos[t-2]", "pos[t-1]", "pos[t]", 
                "pos[t+1]", "pos[t+2]", "pos[t-2]|pos[t-1]", "pos[t-2]|pos[t]", 
                "pos[t-2]|pos[t+1]", "pos[t-2]|pos[t+2]", "pos[t-1]|pos[t]", 
                "pos[t-1]|pos[t+1]", "pos[t-1]|pos[t+2]", "pos[t]|pos[t+1]", 
                "pos[t]|pos[t+2]", "pos[t+1]|pos[t+2]", "pos[t-2]|pos[t-1]|pos[t]", 
                "pos[t-2]|pos[t-1]|pos[t+1]", "pos[t-2]|pos[t-1]|pos[t+2]", "pos[t-2]|pos[t]|pos[t+1]", 
                "pos[t-2]|pos[t]|pos[t+2]", "pos[t-2]|pos[t+1]|pos[t+2]", "pos[t-1]|pos[t]|pos[t+1]", 
                "pos[t-1]|pos[t]|pos[t+2]", "pos[t-1]|pos[t+1]|pos[t+2]", "pos[t]|pos[t+1]|pos[t+2]")
model <- crf(y = crf_train$label, x = crf_train[, attributes], group = crf_train$doc_id, 
             method = "lbfgs", trace = TRUE) 
model
stats <- summary(model, "modeldetails.txt")
stats
plot(stats$iterations$loss)

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
