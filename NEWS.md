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
