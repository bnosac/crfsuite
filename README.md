# Labelling Sequential Data in Natural Language Processing

This repository contains an R package wrapping the CRFsuite C/C++ library (https://github.com/chokkan/crfsuite). It allows

- R users to fit a 1st-order linear-chain Markov **Conditional Random Field** model and to get predictions alongside the model on existing data.
- The focus of the implementation is in the area of **Natural Language Processing** where this R package allows you to easily build and apply models for **named entity recognition, chunking, parts of speech tagging or classification** of any category you have in mind.

For users unfamiliar with Conditional Random Field (CRF) models, you can read this excellent tutorial http://homepages.inf.ed.ac.uk/csutton/publications/crftut-fnt.pdf

## Installation

For installing the development version of this package: `devtools::install_github("bnosac/crfsuite", build_vignettes = TRUE)`

## Data format

In order to build a CRF model, you need to have **sequences of labels** (the hidden state Y) and **attributes of the observations corresponding to the labels** (X). 

- Generally the labels follow this type of scheme B-ORG, I-ORG, E-ORG or B-MYTYPE, I-MYTYPE, E-MYTYPE, O. Indicating the beginng of a certain category (B-), the intermediate part of a certain category (I-) and the ending of a certain category (E-).
- `I went to the New York City District on holidays` would e.g. be labelled as 
`O, O, O, O, B-ORG, I-ORG, I-ORG, I-ORG, O, O`
- The attributes of the observations are mostly something like the term itself, the neighbouring terms, the parts of speech, the neighbouring parts of speech or any specific feature you can extract and which are relevant to your business domain (e.g. the number of numbers in the token, how far is it from the start of the document or end of the document, ...).

As an example, let's get some data in Dutch for doing Named Entity Recognition. It contains 1 row per term and provides entity labels as well as the parts of speech tag for each term. As basic feature enrichment we add the parts of speech tag of the preceding and the next term which we will use later when building the model.

```
library(crfsuite)
library(data.table)
x <- ner_download_modeldata("conll2002-nl")
x <- as.data.table(x)
x <- x[, pos_previous := shift(pos, n = 1, type = "lag"), by = list(doc_id)]
x <- x[, pos_next := shift(pos, n = 1, type = "lead"), by = list(doc_id)]
subset(x, doc_id == 100)
          data doc_id sentence_id         token  pos  label pos_previous pos_next
  1: ned.train    100        8882            EK Pron B-MISC           NA        N
  2: ned.train    100        8882      Magazine    N I-MISC         Pron        N
  3: ned.train    100        8882        Canvas    N  B-ORG            N      Num
  4: ned.train    100        8882         23.45  Num      O            N        N
  5: ned.train    100        8883  Tourjournaal    N B-MISC          Num        N
 ---                                                                             
343: ned.train    100        8916 gepresenteerd    V      O         Punc     Prep
344: ned.train    100        8916          door Prep      O            V        N
345: ned.train    100        8916          Stef    N  B-PER         Prep        N
346: ned.train    100        8916      Wijnants    N  I-PER            N     Punc
347: ned.train    100        8916             . Punc      O            N       NA
```


## Model

### Train your own CRF model

Once you have data which are tagged with your own categories, you can build a CRF model. On the previous data, split it into a training and test dataset

```
x <- as.data.frame(x)
crf_train <- subset(x, data == "ned.train")
crf_test <- subset(x, data == "testa")
```

And start building your model. 

- By default, the **CRF model is trained using L-BFGS with L1/L2 regularization**. 
- In the below example we use the default parameters and decrease the iterations a bit to have a model ready within 30 seconds. 
- Provide the label with the categories (y) and the and the attributes of the observations (x) and indicate what is the sequence group (in this case we take document identifier).

```
opts <- crf_options("lbfgs")
opts <- opts$default
opts$max_iterations <- 25
model <- crf(y = crf_train$label, x = crf_train[, c("token", "pos", "pos_previous", "pos_next")], 
             group = crf_train$doc_id, method = "lbfgs", options = opts, trace = FALSE) 
model
```

### Use the model

You can use the model to get predictions to get the named entity / chunks / categories you trained.

```
scores <- predict(model, newdata = crf_test[, c("token", "pos", "pos_previous", "pos_next")], 
                  group = crf_test$doc_id)
crf_test$entity <- scores$label
```

If you want to combine the different B-ORG, I-ORG, E-ORG labels into one category, you can do as follows

```
example <- as.data.table(crf_test)
groups <- list(
  Location = list(start = "B-LOC", labels = c("B-LOC", "I-LOC", "E-LOC")),
  Organisation =  list(start = "B-ORG", labels = c("B-ORG", "I-ORG", "E-ORG")),
  Person = list(start = "B-PER", labels = c("B-PER", "I-PER", "E-PER")), 
  Misc = list(start = "B-MISC", labels = c("B-MISC", "I-MISC", "E-MISC")))
example <- example[, c("entity_id", "entity") := crf_entities(label, entities = groups), by = "doc_id"]

## Show some Persons
subset(example, entity == "Person")
       data doc_id sentence_id    token pos label pos_previous pos_next entity entity_id
   1: testa    288          20    Peter   N B-PER          Num        N Person        36
   2: testa    288          20    Goris   N I-PER            N        N Person        36
   3: testa    299          37 Isabelle   N B-PER            N        N Person         5
   4: testa    299          37   Durant   N I-PER            N     Punc Person         5
   5: testa    299          46      GSC   N B-PER         Punc     Punc Person        11
  ---                                                                                   
1122: testa    359        2881  Kooning   N I-PER          Art     Punc Person        63
1123: testa    359        2882   Marcel   N B-PER            N        N Person        66
1124: testa    359        2882    Ospel   N I-PER            N        N Person        66
1125: testa    359        2894   Sheryl   N B-PER          Adv        N Person        86
1126: testa    359        2894     Crow   N I-PER            N        V Person        86
```

## Improve the model

When building the model, you need to 

- **tune the parameters of the training algorithm** (L-BFGS, SBD, Averaged Perceptron, Passive/Aggressive, AROW)
- **provide good observation attributes which are specific to your domain**

### Model goodness of fit

In order to identify the parameters of the algorithm, look e.g. at 

```
crf_options("lbfgs")
crf_options("l2sgd")
```

You can obtain information on the model with the following syntax

```
stats <- summary(model, "modeldetails.txt")
str(stats)
plot(stats$iterations$loss, main = "Loss evolution", xlab = "Iteration", ylab = "Loss")
```

If you train the model with different algorithm parameters, you probably are interested to see on the Precision / Recall / F1 statistics to compare them alongside the hyperparameters. You can easily get these with the caret R package.

```
library(caret)
overview <- confusionMatrix(crf_test$entity, crf_test$label, mode = "prec_recall")
overview$overall
overview$byClass[, c("Precision", "Recall", "F1")]
```

### Example with feature engineering

To obtain better models, you need to do feature engineering specific to your business domain.

- In CRF models, the labels (y) obey the Markov property with respect to the graph conditional on the CRF attributes (x). In order to construct attributes (x) of the observations, one looks to neighbouring elements of the tokens (for example 2 words before/after the token). The following example includes features of the neighbouring tokens (namely neighbouring terms and neighbouring parts of speech tags). 
- Please visit the udpipe R package (https://CRAN.R-project.org/package=udpipe) for more information on how to extract e.g. parts of speech tags or other language features of tokens if you want get richer NLP features of the word neighbours.

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
x <- crf_cbind_attributes(x, terms = c("token", "pos"), 
                          by = c("doc_id", "sentence_id"), ngram_max = 3, sep = "|")

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
scores <- predict(model, newdata = crf_test[, attributes], group = crf_test$doc_id)
```

## Support in text mining

Need support in text mining?
Contact BNOSAC: http://www.bnosac.be
