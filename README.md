# Labelling Sequential Data in Natural Language Processing

This repository contains an R package which wraps the CRFsuite C/C++ library (https://github.com/chokkan/crfsuite), allowing the following:

- Fit a **Conditional Random Field** model (1st-order linear-chain Markov) 
- Use the model to get predictions alongside the model on new data
- The focus of the implementation is in the area of Natural Language Processing where this R package allows you to easily build and apply models for **named entity recognition, chunking, parts of speech tagging or classification** of any category you have in mind.

For users unfamiliar with Conditional Random Field (CRF) models, you can read this excellent tutorial http://homepages.inf.ed.ac.uk/csutton/publications/crftut-fnt.pdf

## Installation

For installing the development version of this package: `devtools::install_github("bnosac/crfsuite", build_vignettes = TRUE)`

## Model building and tagging

For detailed documentation on how to build your own CRF tagger for doing NER / Chunking. Look to the vignette.

```{r}
library(crfsuite)
vignette("crfsuite-nlp", package = "crfsuite")
```

#### Short example

```{r}
library(crfsuite)
data(airbnb_chunks, package = "crfsuite")
data(airbnb_tokens, package = "crfsuite")

## Build training data + enrich with neighbouring attributes
x <- merge(airbnb_chunks, airbnb_tokens)
x <- crf_cbind_attributes(x, terms = c("upos", "lemma"), by = "doc_id")
attributes <- grep("upos|lemma", colnames(x), value = TRUE)

## Build model
model <- crf(y = x$chunk_entity, x = x[, attributes], group = x$doc_id, 
             options = list(max_iterations = 25, feature.minfreq = 5, c1 = 0, c2 = 1))

## Prediction functionality
scores <- predict(model, newdata = x[, attributes], group = x$doc_id)
table(scores$label)
         0 B-DISTANCE B-LOCATION   B-PERSON I-DISTANCE I-LOCATION   I-PERSON 
     27646        101        326        452        394        149         10
```


## Build custom CRFsuite models

The package itself does not contain any models to do NER or Chunking. It's a package which facilitates creating **your own CRF model** for doing Named Entity Recognition or Chunking **on your own data** with your **own categories**.

In order to facilitate creating training data on your own data, a shiny app is made available in this R package which allows you to easily tag your own chunks of text, with your own categories. More details can be found in the vignette `vignette("crfsuite-nlp", package = "crfsuite")`.

![](vignettes/app-screenshot.png)


## Support in text mining

Need support in text mining?
Contact BNOSAC: http://www.bnosac.be
