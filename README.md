# crfsuite - Labelling Sequential Data in Natural Language Processing

This repository contains an R package which is an Rcpp wrapper around the CRFsuite C/C++ library (https://github.com/chokkan/crfsuite).

- This allows useRs to fit a 1st-order linear-chain Markov **Conditional Random Field** model with dyad features and to apply it on existing data.
- The focus of the implementation is in the area of **Natural Language Processing** where this R package allows you to easily build and apply models for **named entity recognition, chunking, parts of speech tagging or classification** of any category you have in mind.

## Installation

For installing the development version of this package: `devtools::install_github("bnosac/crfsuite", build_vignettes = TRUE)`

## Example

```
library(crfsuite)
## Get some training data
x <- ner_download_modeldata("conll2002-nl")
crf_train <- subset(x, data == "ned.train")
crf_test <- subset(x, data == "testa")

## Build the CRF model
model <- crf(y = crf_train$label, x = crf_train[, c("token", "pos")], 
             group = crf_train$doc_id, 
             method = "lbfgs", options = list(max_iterations = 5), trace = TRUE) 
model
summary(model, "modeldetails.txt")

## Use the CRF model to label a sequence
scores <- predict(model, 
                  newdata = crf_test[, c("token", "pos")], group = crf_test$doc_id)
head(scores)
table(scores$label == crf_test$label)
```

## Support in text mining

Need support in text mining?
Contact BNOSAC: http://www.bnosac.be
