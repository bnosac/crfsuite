# CHANGES IN crfsuite VERSION 0.4.2

- Drop C++11 specification in Makevars
- Remove CITATION file
- Minor cosmetic changes to remove some Pedantic compiler messages in rcpp_crfsuite.cpp and crfsuite.h
- Comment out rumavl_strerror in rumavl.h as the function is not used and in order to avoid 'warning: function declaration isn't a prototype [-Wstrict-prototypes]' 
- Fix in mem_mgr of ramvl.c caused by update of valgrind 3.21.0 which is now flagging 'realloc with size 0' in packages
- Fix memory leak when extracting the hyperparameters from the model with Trainer::params in crfsuite.hpp

# CHANGES IN crfsuite VERSION 0.4.1

- fixes for R CMD check warning 'a function declaration without a prototype is deprecated in all versions of C [-Wstrict-prototypes]'

# CHANGES IN crfsuite VERSION 0.4

- Add option shape to txt_feature
- Added coefficients.crf to allow to easily extract the coefficients from the model instead of the traditional model dump to disk
- Allow to pass embedding matrix when training and predicting

# CHANGES IN crfsuite VERSION 0.3.4

- Make example conditionally on availability of udpipe

# CHANGES IN crfsuite VERSION 0.3.3

- Incorporate CRFsuite commit a2a1547727985e3aff6a35cffe073f57f0223e9d

# CHANGES IN crfsuite VERSION 0.3.2

- Fix the example of merge.chunkrange

# CHANGES IN crfsuite VERSION 0.3.1

- Change the flexdashboard application by adding links to download the annotations

# CHANGES IN crfsuite VERSION 0.3

- Add functionalities to allow to evaluate a model and tune hyperparameters of a crfsuite model
- Expand txt_feature to allow it to get prefix/suffix of strings
- Shiny app now does not show htmlOutput but textOutput + outputs extra field called 'text_visible' usefull if you change textOutput("ui_txt") to htmlOutput("ui_txt") in the app
- ner_download_modeldata now returns an object of try-error if modeldata could not be downloaded from github. This in order to be more gracefull on CRAN machines in case of internet connectivity issues
- add URL in decscription file
- remove the GNU make as part of the SystemRequirements

# CHANGES IN crfsuite VERSION 0.2

- Fix for as.crf when loaded from file and adding more arguments than just the file
- added txt_feature as a simple feature extraction to identify if a word is capitalised, an email, an url or a number
- src/cqdb/src/lookup3.c, fix address sanitizer issue 

# CHANGES IN crfsuite VERSION 0.1.1

- Change use of posix_memalign to memalign on Solaris

# CHANGES IN crfsuite VERSION 0.1

- Uses CRFsuite (https://github.com/chokkan/crfsuite) version 0.12 commit dc5b6c7b726de90ca63cbf269e6476e18f1dd0d9
- Uses liblbfgs (https://github.com/chokkan/liblbfgs) commit dc5b6c7b726de90ca63cbf269e6476e18f1dd0d9
- Allows to build a CRF model, to predict and to easily add attributes 
- Added flexdashboard app to easily get chunks with labels
